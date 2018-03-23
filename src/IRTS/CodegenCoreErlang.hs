{-# LANGUAGE LambdaCase #-}

module IRTS.CodegenCoreErlang where
-- module IRTS.CodegenCoreErlang (codegenCoreErlang) where

import           Prelude                    hiding (exp)

import           Idris.Core.TT
import           IRTS.CodegenCommon         (CodeGenerator, defunDecls,
                                             exportDecls, outputFile)
import           IRTS.Defunctionalise

import           Control.Monad.Except       (ExceptT, forM_, liftIO, runExceptT,
                                             throwError)
import           Control.Monad.Trans.State  (StateT, execStateT, gets, modify)

import           Data.Char                  (isAlpha, isDigit)
import           Data.List                  (insertBy, partition)
import qualified Data.Map.Strict            as Map
import           Data.Ord                   (comparing)
import           Data.Text                  (Text, pack, unpack)

import           System.Directory           (getPermissions, setOwnerExecutable,
                                             setPermissions)
import           System.Exit                (exitFailure, exitSuccess)
import           System.FilePath            (takeBaseName)

import           Paths_idris_cerl           (getDataFileName)

-- import           IRTS.CodegenCoreErlang.Foreign

import qualified Language.CoreErlang.Pretty as PP
import           Language.CoreErlang.Syntax hiding (App, Const, Var)
import qualified Language.CoreErlang.Syntax as S

-- TODO: Exports

-- TODO: handle debug
-- debugErlang :: Bool
-- debugErlang = False

-- Everything actually happens in `generateCErl`. This is just a bit of
-- glue code.
codegenCoreErlang :: CodeGenerator
codegenCoreErlang ci = do
  let outfile = outputFile ci
  eitherEcg <- runErlCodeGen generateCErl (defunDecls ci) (exportDecls ci)
  case eitherEcg of
    Left err  -> do
      putStrLn ("Error: " ++ err)
      exitFailure
    Right ecg -> do
      _data_dir <- getDataFileName "irts"
      writeFile outfile $ (++ "\n") . unpack . PP.prettyPrint $
        Constr (Module (Atom (pack (takeBaseName outfile)))
                 (header (exports ecg))
                 (Map.elems (fundefs ecg)))
      if null (exportDecls ci)
        then do p <- getPermissions outfile
                setPermissions outfile $ setOwnerExecutable True p
        else return ()
      putStrLn ("Compilation Succeeded: " ++ outfile)
      exitSuccess


header :: [FunName] -> ModHeader
header funcs = ModHeader exports' []
  where exports' = funcs ++ [(Atom "module_info", 0), (Atom "module_info", 1)]

-- | Compiler-generated module_info/0.
emit_module_info0 :: Text -> CErlCG ()
emit_module_info0 modName =
  let a     = [Exp (compiler_generated (Lit (LAtom (Atom modName))))]
      f     = fun [] (get_module_info a)
      fname = (Atom "module_info", 0)
  in  do emitFunDef fname f
         emitExport fname

{-
emitFunDef fname exp =
  case exp of
    Exp (Ann exp' _)  -> modify (action (fdef exp'))
    Exp (Constr exp') -> modify (action (fdef exp'))
  where
    action fdef' ecg = ecg { fundefs = Map.insert fname fdef' (fundefs ecg) }
    fdef             = FunDef (Constr fname) . compiler_generated

-}

-- | Compiler-generated module_info/1.
emit_module_info1 :: Text -> FunDef
emit_module_info1 modName =
  let a = map (Exp . compiler_generated)
              [(Lit (LAtom (Atom modName))), (S.Var "_cor0")]
      f = fun [compiler_generated "_cor0"] (get_module_info a)
  in  funDef "module_info" 1 f

get_module_info :: [Exps] -> Exp
get_module_info =
  ModCall (Exp (compiler_generated (Lit (LAtom (Atom "erlang")))),
            Exp (compiler_generated (Lit (LAtom (Atom "get_module_info")))))

funDef :: Text -> Integer -> Exp -> FunDef
funDef f a exp = FunDef fname (compiler_generated exp)
  where fname = Constr (Atom f, a)

fun :: [Ann VarName] -> Exp -> Exp
fun a exp = Fun a (Exp (compiler_generated exp))

compiler_generated :: a -> Ann a
compiler_generated exp = Ann exp [CLit (LAtom (Atom "compiler_generated"))]

-- exportCompileTag

-- Erlang Codegen State Monad
data ErlCodeGen = ECG
  { fundefs     :: Map.Map FunName FunDef -- name and arity to fun def
  , decls       :: [(Name,DDecl)]
  , records     :: [(Name,Int)]
  , exports     :: [FunName]
  , checked_fns :: Map.Map FunName Int
  } deriving (Show)

initECG :: ErlCodeGen
initECG = ECG { fundefs     = Map.empty
              , decls       = []
              , records     = []
              , exports     = []
              , checked_fns = Map.empty
              }

type CErlCG = StateT ErlCodeGen (ExceptT String IO)

runErlCodeGen
  :: ([(Name,DDecl)] -> [ExportIFace] -> CErlCG ())
  -> [(Name,DDecl)]
  -> [ExportIFace]
  -> IO (Either String ErlCodeGen)
runErlCodeGen ecg ddecls eifaces = runExceptT $
  execStateT (ecg ddecls eifaces) initECG

emitFunDef :: FunName -> Exp -> CErlCG ()
emitFunDef fname exp =
  modify (action (fdef exp))
  where
    action fdef' ecg = ecg { fundefs = Map.insert fname fdef' (fundefs ecg) }
    fdef             = FunDef (Constr fname) . compiler_generated

emitExport :: FunName -> CErlCG ()
emitExport fa = modify (\ecg -> ecg { exports = fa : (exports ecg)})


addRecord :: Name -> Int -> CErlCG ()
addRecord nm ar = do
  recs <- gets records
  let records'   = insertBy (comparing fst) (nm,ar) recs
      action ecg = ecg { records = records' }
  modify action

{-
-- We want to be able to compare the length of constructor arguments
-- to the arity of that record constructor, so this returns the
-- arity. If we can't find the record, then -1 is alright to return,
-- as no list will have that length.
recordArity :: Name -> CErlCG Int
recordArity name = do
  recs <- gets records
  case lookup name recs of
    Just i  -> return i
    Nothing -> return (-1)
-}

isRecord :: Name -> Int -> CErlCG Bool
isRecord nm _ar = do
  recs <- gets records
  case lookup nm recs of
    Just _ar -> return True
    _        -> return False

getVar :: LVar -> CErlCG Exp
getVar (Glob name) = return $ cerlVar name
getVar (Loc  _)    = throwError "Local variables not supported"

{-
getNextCheckedFnName :: FunName -> CErlCG String
getNextCheckedFnName fname = do
  checked <- gets checked_fns
  let insert ecg = ecg { checked_fns = Map.insert fname 1 checked }
      update ecg = ecg { checked_fns = Map.update (Just . (+1)) fname checked }
  case Map.lookup fname checked of
    Nothing -> do
      modify insert
      return $ "checked_" ++ unpack (PP.prettyPrint fname) ++ "_0"
    Just x -> do
      modify update
      return $ "checked_" ++ unpack (PP.prettyPrint fname) ++ "_" ++ show x


{- The Code Generator:

Takes in a Name and a DDecl, and hopefully emits some Forms.

Some Definitions:

- Form : the syntax for top-level Erlang function in an Erlang module

- Module : a group of Erlang functions

- Record : Erlang has n-arity tuples, and they're used for
datastructures, in which case it's usual for the first element in the
tuple to be the name of the datastructure. We'll be using these for
most constructors.

More when I realise they're needed.
-}

-}

generateCErl :: [(Name,DDecl)] -> [ExportIFace] -> CErlCG ()
generateCErl alldecls exportifaces =
  let (ctors, funs) = (isCtor . snd) `partition` alldecls in
  do forM_ ctors $ \(_,DConstructor name _ ar) -> generateCtor name ar
     forM_ funs  $ \(_,DFun name args exp)     -> generateFun name args exp
     generateExports exportifaces
       where
         isCtor (DFun         _ _ _) = False
         isCtor (DConstructor _ _ _) = True

generateExports :: [ExportIFace] -> CErlCG ()
generateExports []   = generateMain
generateExports funs = forM_ funs $
  \(Export name file exports') -> generateExportIFace name file exports'

generateMain :: CErlCG ()
generateMain = do
  cerlExp <- generateExp $ DApp False mainName []
  emitFunDef (Atom "main", 1) cerlExp
    -- "main(_Args) -> \n" ++ dbgStmt ++ erlExp ++ "."
  emitExport (Atom "main", 1)
{-
    where
      dbgStmt = if debugErlang
                then "dbg:tracer(), dbg:p(self(), c), dbg:tpl(?MODULE, dbg:fun2ms(fun(_) -> return_trace() end)),\n"
                else ""
-}

mainName :: Name
mainName  = sMN 0 "runMain"


generateFun :: Name -> [Name] -> DExp -> CErlCG ()
generateFun _ _ DNothing  = return ()
generateFun name args exp = do
  body <- generateExp exp
  let args' = map (Constr . pack . showCG) args
  emitFunDef (cerlAtom name, toInteger (length args)) (fun args' body)

generateCtor :: Name -> Int -> CErlCG ()
generateCtor = addRecord

generateExportIFace :: Name -> String -> [Export] -> CErlCG ()
generateExportIFace _ _ = mapM_ generateExport

generateExport :: Export -> CErlCG ()
generateExport (ExportData _) = return () -- Literally just string names of types, can't do anything with them.
generateExport (ExportFun fn (FStr _enm) _ret _args) =
  liftIO (print (PP.prettyPrint (cerlAtom fn))) >> return ()
  --emitForm (enm, length args) $
  --  checkedExport enm
generateExport x = throwError $ "generateExport " ++ show x

generateExp :: DExp -> CErlCG Exp
generateExp (DV lv)              = getVar lv

generateExp (DApp _ name exprs)  = do
  res    <- isRecord name (length exprs)
  exprs' <- mapM generateExp exprs
  if res
    then specialCaseCtor name (map (Exp . Constr) exprs') -- FIXME: this is lame
    else return $ cerlCall (pack (showCG name)) (map (Exp . Constr) exprs')

generateExp (DLet vn exp inExp) = do
  exp'   <- generateExp exp
  inExp' <- generateExp inExp
  return $ S.Let ([pack (showCG vn)], Exp (Constr exp')) (Exp (Constr inExp'))
  --  ++ " = begin " ++ exp' ++ " end, "++ inExp'

-- These are never generated by the compiler right now
generateExp (DUpdate _ exp)     = generateExp exp

generateExp (DProj exp n)       = do
  exp'   <- generateExp exp
  return $ cerlCallIRTS "project" [ Exp (Constr exp')
                                  , Exp (Constr (Lit (LString (pack (show n)))))
                                  ]

generateExp (DC _ _ name exprs) = do
  res    <- isRecord name (length exprs)
  exprs' <- mapM generateExp exprs
  if res
    then specialCaseCtor name (map (Exp . Constr) exprs')
    else throwError $ "Constructor not found: " ++ show name ++ " with " ++ show (length exprs) ++ "arguments"

{- FIXME
generateExp (DCase _  exp alts)    = generateCase exp alts
generateExp (DChkCase exp alts)    = generateCase exp alts
-}

generateExp (DConst c)             = generateConst c >>=
  \case CLit c' -> return $ Lit c'
        x       -> throwError $ "generateExp (DConst " ++ show x ++ ")" -- FIXME

-- FIXME:
-- generateExp (DOp op exprs)         = mapM generateExp exprs >>= generatePrim op

generateExp  DNothing              = return $ Lit (LAtom (Atom "undefined"))
generateExp (DError s)             = return $
  cerlCallMFA "erlang" "error" [Exp (Constr (Lit (LString (pack s))))]

-- FIXME: generateExp (DForeign ret nm args) = generateForeign ret nm args

generateExp x = throwError $ "generateExp " ++ show x


generateCase :: DExp -> [DAlt] -> CErlCG Exp
generateCase _ _ = undefined

generateForeign :: FDesc -> FDesc -> [(FDesc,DExp)] -> CErlCG Exp
generateForeign _ _ _ = undefined

{-
-- Case Statements
generateCase :: DExp -> [DAlt] -> CErlCG String

-- In the naive case, lots of case statements that look like the following get generated:
--
-- case bool_cast(x OP y) of
--   0 -> false;
--   _ -> true
-- end
--
-- This is annoying, as bool_cast has already changed `x OP y` from
-- returning a bool to returning an integer, so we special-case these
-- case statements into just generating the bool again.
generateCase (DOp op exprs) [DConstCase (I 0) false, DDefaultCase true]
  | isBoolOp op && isFalseCtor false && isTrueCtor true = do exprs' <- mapM generateExp exprs
                                                             simpleBoolOp op exprs'
  where isFalseCtor (DC _ _ (NS (UN "False") ["Bool", "Prelude"]) []) = True
        isFalseCtor _ = False
        isTrueCtor (DC _ _ (NS (UN "True") ["Bool", "Prelude"])   []) = True
        isTrueCtor _ = False
generateCase expr alts = do expr' <- generateExp expr
                            alts' <- mapM generateCaseAlt alts
                            return $ "case " ++ expr' ++ " of\n" ++ (";\n" `intercalate` alts') ++ "\nend"

-- Case Statement Clauses
generateCaseAlt :: DAlt -> CErlCG String
generateCaseAlt (DConCase _ name args expr) = do
  res <- isRecord name (length args)
  let args' = map erlVar args
  if res
    then do expr' <- generateExp expr
            ctor <- specialCaseCtor name args'
            return $ ctor ++ " -> " ++ expr'
    else throwError "No Constructor to Match With"
generateCaseAlt (DConstCase con expr)       = do
  con'  <- generateConst con
  expr' <- generateExp expr
  return $ con' ++ " -> " ++ expr'
generateCaseAlt (DDefaultCase expr) = generateExp expr >>= return . ("_ -> " ++)


-- Foreign Calls
--
generateForeign :: FDesc -> FDesc -> [(FDesc,DExp)] -> CErlCG String
generateForeign CErl_Atom (FStr "list_to_atom") [(CErl_Str,DConst (Str s))] =
  return $ strAtom s
generateForeign rety (FStr nm) args =
  do checkedNm <- getNextCheckedFnName nm (length args)
     args'   <- mapM (generateExp . snd) args
     emitFunDef (checkedNm, length args) $
       checkedFnCall checkedNm nm rety (map fst args)
     return $ cerlCall checkedNm args'
generateForeign _ _ _ = throwError "oh no!"
-}

-- Some Notes on Constants
--
-- - All Erlang's numbers are arbitrary precision. The VM copes with
-- what size they really are underneath, including whether they're a
-- float.
--
-- - Characters are just numbers. However, there's also a nice syntax
-- for them, which is $<char> is the number of that character. So, if
-- the char is printable, it's best to use the $<char> notation than
-- the number.
--
-- - Strings are actually lists of numbers. However the nicer syntax
-- is within double quotes. Some things will fail, but it's just
-- easier to assume all strings are full of printables, if they're
-- constant.
generateConst :: Const -> CErlCG S.Const
generateConst TheWorld = return $ CLit (LAtom (Atom "the_world"))
generateConst c | constIsType c = return $ CLit (LAtom (Atom (pack (show c))))
generateConst (I i)   = return $ CLit (LInt (toInteger i))
generateConst (BI i)  = return $ CLit (LInt (toInteger i))
generateConst (B8 w)  = return $ CLit (LInt (toInteger w))
generateConst (B16 w) = return $ CLit (LInt (toInteger w))
generateConst (B32 w) = return $ CLit (LInt (toInteger w))
generateConst (B64 w) = return $ CLit (LInt (toInteger w))
generateConst (Fl f)  = return $ CLit (LFloat f)
                     -- Accurate Enough for now
{- FIXME:
generateConst (Ch c) | c == '\\'  = return "$\\\\"
                     | isPrint c = return ['$',c]
                     | otherwise = return $ show (fromEnum c)
                      -- Accurate Enough for Now
-}
{- FIXME:
generateConst (Str s) | any (== '\\') s = do chars <- sequence $ map (generateConst . Ch) s
                                             return $ "[" ++ (", " `intercalate` chars) ++ "]"
                      | all isPrint s = return $ show s
                      | otherwise = do chars <- sequence $ map (generateConst . Ch) s
                                       return $ "[" ++ (", " `intercalate` chars) ++ "]"
-}
generateConst c = throwError $ "Unknown Constant " ++ show c


-- Some Notes on Primitive Operations
--
-- - Official Docs:
-- http://www.erlang.org/doc/reference_manual/expressions.html#id78907
-- http://www.erlang.org/doc/reference_manual/expressions.html#id78646
--
-- - Oh look, because we only have one number type, all mathematical
-- operations are really easy. The only thing to note is this: `div`
-- is explicitly integer-only, so is worth using whenever integer
-- division is asked for (to avoid everything becoming floaty). '/' is
-- for any number, so we just use that on floats.
--
--
generatePrim :: PrimFn -> [Exps] -> CErlCG Exp
generatePrim (LPlus _)       [x,y] = return $ cerlBinOp "+" x y
generatePrim (LMinus _)      [x,y] = return $ cerlBinOp "-" x y
generatePrim (LTimes _)      [x,y] = return $ cerlBinOp "*" x y
generatePrim (LUDiv _)       [x,y] = return $ cerlBinOp "div" x y
generatePrim (LSDiv ATFloat) [x,y] = return $ cerlBinOp "/" x y
generatePrim (LSDiv _)       [x,y] = return $ cerlBinOp "div" x y
generatePrim (LURem _)       [x,y] = return $ cerlBinOp "rem" x y
generatePrim (LSRem _)       [x,y] = return $ cerlBinOp "rem" x y
generatePrim (LAnd _)        [x,y] = return $ cerlBinOp "band" x y
generatePrim (LOr _)         [x,y] = return $ cerlBinOp "bor" x y
generatePrim (LXOr _)        [x,y] = return $ cerlBinOp "bxor" x y
-- FIXME:
-- generatePrim (LCompl _)      [x]   = return $ cerlBinOp "bnot" "" x  -- hax
generatePrim (LSHL _)        [x,y] = return $ cerlBinOp "bsl" x y
generatePrim (LASHR _)       [x,y] = return $ cerlBinOp "bsr" x y
generatePrim (LLSHR _)       [x,y] = return $ cerlBinOp "bsr" x y -- using an arithmetic shift when we should use a logical one.


generatePrim (LEq _)         [x,y] = return $ cerlBoolOp "=:=" x y
generatePrim (LLt _)         [x,y] = return $ cerlBoolOp "<" x y
generatePrim (LLe _)         [x,y] = return $ cerlBoolOp "=<" x y
generatePrim (LGt _)         [x,y] = return $ cerlBoolOp ">" x y
generatePrim (LGe _)         [x,y] = return $ cerlBoolOp ">=" x y
generatePrim (LSLt _)        [x,y] = return $ cerlBoolOp "<" x y
generatePrim (LSLe _)        [x,y] = return $ cerlBoolOp "=<" x y
generatePrim (LSGt _)        [x,y] = return $ cerlBoolOp ">" x y
generatePrim (LSGe _)        [x,y] = return $ cerlBoolOp ">=" x y

-- FIXME:
generatePrim (LSExt _ _)     [x]   = return . (\(Exp (Constr x')) -> x') $ x -- Not sure if correct
generatePrim (LZExt _ _)     [x]   = return . (\(Exp (Constr x')) -> x') $ x -- Not sure if correct
generatePrim (LTrunc _ _)    [x]   = return . (\(Exp (Constr x')) -> x') $ x -- Not sure if correct

generatePrim (LIntFloat _)   [x]   = return $ cerlBinOp "+" x (Exp (Constr (Lit (LFloat 0.0))))
generatePrim (LFloatInt _)   [x]   = return $ cerlCallMFA "erlang" "trunc" [x]
generatePrim (LIntStr _)     [x]   = return $ cerlCallMFA "erlang" "integer_to_list" [x]
generatePrim (LStrInt _)     [x]   = return $ cerlCallMFA "erlang" "list_to_integer" [x]
generatePrim (LFloatStr)     [x]   = return $ cerlCallMFA "erlang" "float_to_list" [x, opts]
  where
    literal = Exp . Constr . Lit
    twenty = literal (LInt 20)
    atom' = literal . LAtom . Atom
    tuple = Exp . Constr. Tuple
    l = Exp . Constr . List . L
    ll xs = Exp . Constr . List . LL xs . l
    opts = ll [atom' "compact"] [tuple [atom' "decimals", twenty]]
generatePrim (LStrFloat)     [x]   = return $ cerlCallMFA "erlang" "list_to_float" [x]

generatePrim (LChInt _)      [x]   = return . (\(Exp (Constr x')) -> x') $ x -- Chars are just Integers anyway.
generatePrim (LIntCh _)      [x]   = return . (\(Exp (Constr x')) -> x') $ x
generatePrim (LBitCast _ _)  [x]   = return . (\(Exp (Constr x')) -> x') $ x

generatePrim (LFExp)         [x]   = return $ cerlCallMFA  "math"  "exp"  [x]
generatePrim (LFLog)         [x]   = return $ cerlCallMFA  "math"  "log"  [x]
generatePrim (LFSin)         [x]   = return $ cerlCallMFA  "math"  "sin"  [x]
generatePrim (LFCos)         [x]   = return $ cerlCallMFA  "math"  "cos"  [x]
generatePrim (LFTan)         [x]   = return $ cerlCallMFA  "math"  "tan"  [x]
generatePrim (LFASin)        [x]   = return $ cerlCallMFA  "math"  "asin" [x]
generatePrim (LFACos)        [x]   = return $ cerlCallMFA  "math"  "acos" [x]
generatePrim (LFATan)        [x]   = return $ cerlCallMFA  "math"  "atan" [x]
generatePrim (LFSqrt)        [x]   = return $ cerlCallMFA  "math"  "sqrt" [x]
generatePrim (LFFloor)       [x]   = return $ cerlCallIRTS "ceil"         [x]
generatePrim (LFCeil)        [x]   = return $ cerlCallIRTS "floor"        [x]
generatePrim (LFNegate)      [x]   = return $ cerlCallMFA  "erlang" "-"   [x]

generatePrim (LStrHead)      [x]   = return $ cerlCallMFA "erlang" "hd" [x]
generatePrim (LStrTail)      [x]   = return $ cerlCallMFA "erlang" "tl" [x]
generatePrim (LStrCons)      [x,y] = return $ List (LL [x] y) -- TODO: Check this
generatePrim (LStrIndex)     [x,y] = return $ cerlCallIRTS "str_index" [x,y]
generatePrim (LStrRev)       [x]   = return $ cerlCallMFA "lists" "reverse" [x]
generatePrim (LStrConcat)    [x,y] = return $ cerlBinOp "++" x y
generatePrim (LStrLt)        [x,y] = return $ cerlBoolOp "<" x y
generatePrim (LStrEq)        [x,y] = return $ cerlBoolOp "=:=" x y
generatePrim (LStrLen)       [x]   = return $ cerlCallMFA "erlang" "length" [x]

generatePrim (LReadStr)      [_]   = return $ cerlCallIRTS "read_str" []
generatePrim (LWriteStr)     [_,s] = return $ cerlCallIRTS "write_str" [s]

generatePrim (LSystemInfo)    _    = throwError "System Info not supported" -- TODO

-- FIXME:
-- generatePrim (LFork)         [e]   = return $ "spawn(fun() -> "++ cerlCall (cerlAtom evalName) [e] ++" end)"

-- FIXME:
-- generatePrim (LPar)          [e]   = return e

-- FIXME:
-- generatePrim (LExternal nm)  args  = generateExternalPrim nm args

generatePrim p a = do liftIO . putStrLn $ "No Primitive: " ++ show p ++ " on " ++ show (length a) ++ " args."
                      throwError "generatePrim: Unknown Op, or incorrect arity"

{-
generateExternalPrim :: Name -> [Exps] -> CErlCG Exp
generateExternalPrim nm _ | nm == sUN "prim__stdin"  = return $ "standard_io"
                          | nm == sUN "prim__stdout" = return $ "standard_io"
                          | nm == sUN "prim__stderr" = return $ "standard_io"
                          | nm == sUN "prim__vm"     = return $ "undefined"
                          | nm == sUN "prim__null"   = return $ "undefined"
generateExternalPrim nm [_,h]     | nm == sUN "prim__readFile"     = return $ cerlCallIRTS "read_file" [h]
generateExternalPrim nm [_,h,s]   | nm == sUN "prim__writeFile"    = return $ cerlCallIRTS "write_file" [h,s]
generateExternalPrim nm [p1,p2] | nm == sUN "prim__eqPtr"        = return $ cerlCallIRTS "ptr_eq" [p1,p2]
generateExternalPrim nm [p1,p2] | nm == sUN "prim__eqManagedPtr" = return $ cerlCallIRTS "ptr_eq" [p1,p2]
generateExternalPrim nm [p,l]   | nm == sUN "prim__registerPtr"  = return $ cerlCallIRTS "register_ptr" [p,l]
generateExternalPrim nm args = do liftIO . putStrLn $ "Unknown External Primitive: " ++ show nm ++ " on " ++ show (length args) ++ " args."
                                  throwError "generatePrim: Unknown External Primitive"
-}

cerlBinOp :: Text -> Exps -> Exps -> Exp
cerlBinOp op a b = cerlCallMFA "erlang" op [a,b]

-- Erlang Atoms can contain quite a lot of chars, so let's see how they cope
cerlAtom :: Name -> Atom
cerlAtom n = Atom . pack $ showCG n

atom :: String -> Atom
atom = Atom . pack

strAtom :: String -> String
strAtom = unpack . PP.prettyPrint . atom

{-
-- strAtom s = "\'" ++ concatMap atomchar s ++ "\'"
--   where atomchar '\'' = "\\'"
--         atomchar '\\' = "\\\\"
--         atomchar '{'  = ""
--         atomchar '}'  = ""
--         atomchar x | isPrint x = [x]
--                    | otherwise = "_" ++ show (fromEnum x) ++ "_"
-}

-- Erlang Variables have a more restricted set of chars, and must
-- start with a capital letter (erased can start with an underscore)
cerlVar :: Name -> Exp
cerlVar n = S.Var . pack $ 'I':(concatMap varchar (showCG n))
  where
    varchar '_' = "_"
    varchar '{' = ""
    varchar '}' = ""
    varchar  x  | isAlpha x = [x]
                | isDigit x = [x]
                | otherwise = "_" ++ show (fromEnum x) ++ "_"

cerlCall :: Text -> [Exps] -> Exp
cerlCall f a = S.App (funName f a) a

funName :: Text -> [Exps] -> Exps
funName f a = Exp (Constr (FunName (Atom f, toInteger (length a))))

cerlCallMFA :: Text -> Text -> [Exps] -> Exp
cerlCallMFA m f a = ModCall (m', funName f a) a
  where m' = Exp (Constr (Lit (LAtom (Atom m))))

-- args_ :: [String] -> [Exps]
-- args_ = map (Exp . Constr . S.Var . pack)


cerlCallIRTS :: Text -> [Exps] -> Exp
cerlCallIRTS = cerlCallMFA "idris_erlang_rts"

cerlBoolOp :: Text -> Exps -> Exps -> Exp
cerlBoolOp op x y = cerlCallIRTS "bool_cast"
                  $ [Exp (Constr (cerlBinOp op x y))]

isBoolOp :: PrimFn -> Bool
isBoolOp (LEq   _) = True
isBoolOp (LLt   _) = True
isBoolOp (LLe   _) = True
isBoolOp (LGt   _) = True
isBoolOp (LGe   _) = True
isBoolOp (LSLt  _) = True
isBoolOp (LSLe  _) = True
isBoolOp (LSGt  _) = True
isBoolOp (LSGe  _) = True
isBoolOp (LStrLt)  = True
isBoolOp (LStrEq)  = True
isBoolOp _         = False

simpleBoolOp :: PrimFn -> [Exps] -> CErlCG Exp
simpleBoolOp (LEq    _) [x,y] = return $ cerlBinOp "=:=" x y
simpleBoolOp (LLt    _) [x,y] = return $ cerlBinOp "<"   x y
simpleBoolOp (LLe    _) [x,y] = return $ cerlBinOp "=<"  x y
simpleBoolOp (LGt    _) [x,y] = return $ cerlBinOp ">"   x y
simpleBoolOp (LGe    _) [x,y] = return $ cerlBinOp ">="  x y
simpleBoolOp (LSLt   _) [x,y] = return $ cerlBinOp "<"   x y
simpleBoolOp (LSLe   _) [x,y] = return $ cerlBinOp "=<"  x y
simpleBoolOp (LSGt   _) [x,y] = return $ cerlBinOp ">"   x y
simpleBoolOp (LSGe   _) [x,y] = return $ cerlBinOp ">="  x y
simpleBoolOp (LStrLt)   [x,y] = return $ cerlBinOp "<"   x y
simpleBoolOp (LStrEq)   [x,y] = return $ cerlBinOp "=:=" x y
simpleBoolOp _ _ =
  throwError "Unknown Boolean Primitive Operation. This should never happen."

-- This is where we special case various constructors.
--
-- * Prelude.List.Nil gets turned into Lit LNil
-- * Prelude.List.(::) gets turned into List (LL [hd] tl)
-- * MkUnit () gets turned into Tuple []
-- * Prelude.Bool.True gets turned into true
-- * Prelude.Bool.False gets turned into false
--
specialCaseCtor :: Name -> [Exps] -> CErlCG Exp
specialCaseCtor (NS (UN "Nil") ["List", "Prelude"])   []      = return $
  Lit LNil
specialCaseCtor (NS (UN "::")  ["List", "Prelude"])   [hd,tl] = return $
  List (LL [hd] tl)
specialCaseCtor (UN "MkUnit")                         []      = return $
  Tuple []
specialCaseCtor (NS (UN "True")  ["Bool", "Prelude"]) []      = return $
  Lit (LAtom (Atom "true"))
specialCaseCtor (NS (UN "False") ["Bool", "Prelude"]) []      = return $
  Lit (LAtom (Atom "false"))
specialCaseCtor nm                                    args    = return $
  Tuple (Exp (Constr (Lit (LAtom (cerlAtom nm)))) : args)
