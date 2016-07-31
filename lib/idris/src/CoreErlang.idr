module CoreErlang

%access public export

--------- The Core Erlang FFI
namespace FFI_CErl
  data Raw : Type -> Type where
    MkRaw : (x : t) -> FFI_CErl.Raw t

  data CErlFn : Type -> Type where
    MkCErlFn : (x : t) -> CErlFn t

  ||| A Core Erlang Atom.
  data Atom : Type

  mutual
    ||| Core Erlang integer types.
    data CErl_IntTypes   : Type -> Type where
         CErl_IntChar    : CErl_IntTypes Char
         CErl_IntNative  : CErl_IntTypes Int

    ||| Core Erlang function types.
    data CErl_FnTypes : Type -> Type where
         CErl_Fn      : CErl_Types s -> CErl_FnTypes t -> CErl_FnTypes (s -> t)
         CErl_FnIO    : CErl_Types t -> CErl_FnTypes (IO' l t)
         CErl_FnBase  : CErl_Types t -> CErl_FnTypes t

    ||| Core Erlang foreign types.
    data CErl_Types : Type -> Type where
         ||| Integer | Char
         CErl_IntT  : CErl_IntTypes i -> CErl_Types i
         ||| Float
         CErl_Float : CErl_Types Double
         ||| Atom
         CErl_Atom  : CErl_Types Atom
         ||| String
         CErl_Str   : CErl_Types String

         CErl_Ptr   : CErl_Types Ptr

         CErl_Unit  : CErl_Types ()

         CErl_Any   : CErl_Types (FFI_CErl.Raw a)

         ||| List
         CErl_List  : CErl_Types a -> CErl_Types (List a)
         ||| Fun
         CErl_FnT   : CErl_Types t -> CErl_Types (CErlFn t)

%used FFI_CErl.MkRaw x
%used MkCErlFn x

||| The Core Erlang FFI. The strings naming functions in this API are
||| Core Erlang code snippets, into which the arguments are substituted
||| for the placeholders `%0`, `%1,` etc.
FFI_CErl : FFI
FFI_CErl = MkFFI CErl_Types String String

||| Interactive Core Erlang programs,
||| describing I/O actions and returning a value.
||| @ res The result type of the program.
CErl_IO : (res : Type) -> Type
CErl_IO = IO' FFI_CErl

%inline
cerlcall : (fname : String)
         -> (ty : Type)
         -> {auto fty : FTy FFI_CErl [] ty}
         -> ty
cerlcall fname ty = foreign FFI_CErl fname ty

%inline
CErl_Export : Type
CErl_Export = FFI_Export FFI_CErl "" []

||| A Core Erlang process ID (pid).
CErlPid : Type
CErlPid = Ptr

||| Return the atom whose text representation is `str`, i.e.
||| ```erlang
||| fun (_cor0) -> call 'erlang':'list_to_atom' (_cor0)
||| ```
||| @ str A text representation of an atom.
atom : (str : String) -> CErl_IO Atom
atom = cerlcall "'erlang':'list_to_atom'" (String -> CErl_IO Atom)


-- Modified from ErlPrelude.

data EFile = EHandle Ptr

namespace CErl
  stdin : EFile
  stdin = EHandle prim__stdin

  stdout : EFile
  stdout = EHandle prim__stdout

  stderr : EFile
  stderr = EHandle prim__stderr

  openFile : String -> Mode -> CErl_IO EFile
  openFile filename mode = pure EHandle <*>  open filename (modeStr mode)
    where
    modeStr : Mode -> String
    modeStr Read      = "r"
    modeStr Write     = "w"
    modeStr ReadWrite = "rw"

    open : String -> String -> CErl_IO Ptr
    open = cerlcall "idris_cerl:file_open" (String -> String -> CErl_IO Ptr)

  closeFile : EFile -> CErl_IO ()
  closeFile (EHandle p) = close p *> pure ()
    where
      close : Ptr -> CErl_IO Int
      close = cerlcall "'idris_cerl':'file_close'" (Ptr -> CErl_IO Int)

  fgetc' : EFile -> CErl_IO (Maybe Char)
  fgetc' (EHandle h) = do c <- getChar h
                          pure $ if c < 0 then Nothing else Just (cast c)
    where
      getChar : Ptr -> CErl_IO Int
      getChar = cerlcall "idris_cerl:read_chr" (Ptr -> CErl_IO Int)

  fgetc : EFile -> CErl_IO Char
  fgetc (EHandle h) = pure cast <*> getChar h
    where
      getChar : Ptr -> CErl_IO Int
      getChar = cerlcall "idris_cerl:read_chr" (Ptr -> CErl_IO Int)

  fread : EFile -> CErl_IO String
  fread (EHandle h) = prim_fread h

  fwrite : EFile -> String -> CErl_IO ()
  fwrite (EHandle h) s = writeFile h s *> pure ()
    where
      writeFile : Ptr -> String -> CErl_IO Int
      writeFile = cerlcall "idris_cerl:write_file" (Ptr -> String -> CErl_IO Int)

  feof : EFile -> CErl_IO Bool
  feof (EHandle h) = pure (/= 0) <*> fileEOF h
    where
      fileEOF : Ptr -> CErl_IO Int
      fileEOF = cerlcall "idris_cerl:file_eof" (Ptr -> CErl_IO Int)

  fflush : EFile -> CErl_IO ()
  fflush (EHandle h) = fileFlush h *> pure ()
    where
      fileFlush : Ptr -> CErl_IO Int
      fileFlush = cerlcall "idris_cerl:file_flush" (Ptr -> CErl_IO Int)

  putChar : Char -> CErl_IO ()
  putChar c = putStr' (singleton c)

  getChar : CErl_IO Char
  getChar = fgetc stdin

  nullStr : String -> CErl_IO Bool
  nullStr s = pure (/= 0) <*> strIsNull s
    where
      strIsNull : String -> CErl_IO Int
      strIsNull = cerlcall "idris_cerl:str_null" (String -> CErl_IO Int)

  nullPtr : Ptr -> CErl_IO Bool
  nullPtr p = pure (/= 0) <*> isNull p
    where
      isNull : Ptr -> CErl_IO Int
      isNull = cerlcall "idris_cerl:ptr_null" (Ptr -> CErl_IO Int)

  eqPtr : Ptr -> Ptr -> CErl_IO Bool
  eqPtr x y = pure (/= 0) <*> ptrIsEq x y
    where
      ptrIsEq : Ptr -> Ptr -> CErl_IO Int
      ptrIsEq = cerlcall "idris_cerl:ptr_eq" (Ptr -> Ptr -> CErl_IO Int)

  validFile : EFile -> CErl_IO Bool
  validFile (EHandle h) = pure not <*> nullPtr h

  partial
  readFile : String -> CErl_IO String
  readFile fn = do f <- openFile fn Read
                   c <- readFile' f ""
                   closeFile f
                   pure c
    where
      partial
      readFile' : EFile -> String -> CErl_IO String
      readFile' f contents = do res <- feof f
                                if   not res
                                then fread f >>= readFile' f . (contents ++)
                                else pure contents
