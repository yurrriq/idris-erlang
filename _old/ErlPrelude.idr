module ErlPrelude

%access public export

data ErlFn : Type -> Type where
  MkErlFun : (x : t) -> ErlFn t
%used MkErlFun x

data ErlRaw : Type -> Type where
  MkERaw : (x:t) -> ErlRaw t
%used MkERaw x

public export data Atom : Type

data Erl_NumTypes: Type -> Type where
  Erl_IntChar    : Erl_NumTypes Char
  Erl_IntNative  : Erl_NumTypes Int
  Erl_Double     : Erl_NumTypes Double

mutual
  data Erl_FunTypes : Type -> Type where
    Erl_Fun     : Erl_Types s -> Erl_FunTypes t -> Erl_FunTypes (s -> t)
    Erl_FunIO   : Erl_Types t -> Erl_FunTypes (EIO t)
    Erl_FunBase : Erl_Types t -> Erl_FunTypes t

  data Erl_Types : Type -> Type where
    Erl_Str  : Erl_Types String
    Erl_Atom : Erl_Types Atom
    Erl_Ptr  : Erl_Types Ptr
    Erl_Unit : Erl_Types ()
    Erl_List : Erl_Types a -> Erl_Types (List a)
    Erl_FunT : Erl_FunTypes a -> Erl_Types (ErlFn a)
    Erl_NumT : Erl_NumTypes t -> Erl_Types t
    Erl_Raw  : Erl_Types (ErlRaw a)

  FFI_Erl : FFI
  FFI_Erl = MkFFI Erl_Types String String

  -- Make your "Old MacDonald" jokes here please
  EIO : Type -> Type
  EIO = IO' FFI_Erl

%inline
erlcall : (fname : String) -> (ty : Type) -> {auto fty : FTy FFI_Erl [] ty} -> ty
erlcall fname ty = foreign FFI_Erl fname ty

%inline
Erl_Export : Type
Erl_Export = FFI_Export FFI_Erl "" []

ErlPid : Type
ErlPid = Ptr

-- Annoyingly, the File struct is abstract so we can't use it. I guess
-- this helps prevent people mixing the two kinds of files... not that
-- it would even be possible.
public export data EFile = EHandle Ptr

namespace Erl
  stdin : EFile
  stdin = EHandle prim__stdin

  stdout : EFile
  stdout = EHandle prim__stdout

  stderr : EFile
  stderr = EHandle prim__stderr

  openFile : String -> Mode -> EIO EFile
  openFile filename mode = do p <- open filename (modeStr mode)
                              return (EHandle p)
    where modeStr : Mode -> String
          modeStr Read = "r"
          modeStr Write = "w"
          modeStr ReadWrite = "rw"

          open : String -> String -> EIO Ptr
          open = foreign FFI_Erl "idris_erlang_rts:file_open" (String -> String -> EIO Ptr)


  closeFile : EFile -> EIO ()
  closeFile (EHandle p) = do x <- close p
                             return ()
    where close : Ptr -> EIO Int
          close = foreign FFI_Erl "idris_erlang_rts:file_close" (Ptr -> EIO Int)


  fgetc' : EFile -> EIO (Maybe Char)
  fgetc' (EHandle h) = do c <- getChar h
                          if (c < 0)
                          then return Nothing
                          else return (Just (cast c))
    where getChar : Ptr -> EIO Int
          getChar = foreign FFI_Erl "idris_erlang_rts:read_chr" (Ptr -> EIO Int)

  fgetc : EFile -> EIO Char
  fgetc (EHandle h) = do c <- getChar h
                         return (cast c)
    where getChar : Ptr -> EIO Int
          getChar = foreign FFI_Erl "idris_erlang_rts:read_chr" (Ptr -> EIO Int)


  fread : EFile -> EIO String
  fread (EHandle h) = prim_fread h

  fwrite : EFile -> String -> EIO ()
  fwrite (EHandle h) s = do writeFile h s
                            return ()
    where writeFile : Ptr -> String -> EIO Int
          writeFile = foreign FFI_Erl "idris_erlang_rts:write_file" (Ptr -> String -> EIO Int)

  feof : EFile -> EIO Bool
  feof (EHandle h) = do res <- fileEOF h
                        return (res /= 0)
    where fileEOF : Ptr -> EIO Int
          fileEOF = foreign FFI_Erl "idris_erlang_rts:file_eof" (Ptr -> EIO Int)

  fflush : EFile -> EIO ()
  fflush (EHandle h) = do fileFlush h
                          return ()
    where fileFlush : Ptr -> EIO Int
          fileFlush = foreign FFI_Erl "idris_erlang_rts:file_flush" (Ptr -> EIO Int)


  putChar : Char -> EIO ()
  putChar c = putStr' (singleton c)

  getChar : EIO Char
  getChar = fgetc stdin

  nullStr : String -> EIO Bool
  nullStr s = do res <- strIsNull s
                 return (res /= 0)
    where strIsNull : String -> EIO Int
          strIsNull = foreign FFI_Erl "idris_erlang_rts:str_null" (String -> EIO Int)


  nullPtr : Ptr -> EIO Bool
  nullPtr p = do res <- isNull p
                 return (res /= 0)
    where isNull : Ptr -> EIO Int
          isNull = foreign FFI_Erl "idris_erlang_rts:ptr_null" (Ptr -> EIO Int)

  eqPtr : Ptr -> Ptr -> EIO Bool
  eqPtr x y = do res <- ptrIsEq x y
                 return (res /= 0)
    where ptrIsEq : Ptr -> Ptr -> EIO Int
          ptrIsEq = foreign FFI_Erl "idris_erlang_rts:ptr_eq" (Ptr -> Ptr -> EIO Int)

  validFile : EFile -> EIO Bool
  validFile (EHandle h) = do res <- nullPtr h
                             return (not res)

  partial
  readFile : String -> EIO String
  readFile fn = do f <- openFile fn Read
                   c <- readFile' f ""
                   closeFile f
                   return c
    where
      partial
      readFile' : EFile -> String -> EIO String
      readFile' f contents = do res <- feof f
                                if (not res)
                                then do l <- fread f
                                        readFile' f (contents ++ l)
                                else return contents

atom : String -> EIO Atom
atom = foreign FFI_Erl "list_to_atom" (String -> EIO Atom)
