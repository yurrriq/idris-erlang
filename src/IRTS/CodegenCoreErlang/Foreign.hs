{-# LANGUAGE OverloadedStrings #-}

module IRTS.CodegenCoreErlang.Foreign where

import Prelude hiding (exp)

import Data.List (intercalate)
import Data.Maybe
import Data.Text (Text)
-- import Data.Text (Text, pack, unpack)

import Idris.Core.TT
import IRTS.Lang
-- import IRTS.CodegenCommon
-- import IRTS.Defunctionalise

-- import qualified Language.CoreErlang.Pretty     as PP
import           Language.CoreErlang.Syntax     hiding (App, Const, Var)
import qualified Language.CoreErlang.Syntax     as S


evalName, applyName :: Name
evalName  = sMN 0 "EVAL"
applyName = sMN 0 "APPLY"

data CErlT = CErlString
           | CErlAtom
           | CErlUnit
           | CErlInt
           | CErlDouble
           -- | Something Core Erlang understands, and Idris doesn't
           | CErlPtr
           -- | Something Idris understands, and Core Erlang doesn't
           | CErlRaw
           | CErlList CErlT
           | CErlFn Bool CErlT [CErlT] -- (x,y,z) -> a (Bool is if a is an IO thing we should unwrap)
           deriving (Show, Eq)

cerlFun :: CErlT -> Bool
cerlFun (CErlFn _ _ _) = True
cerlFun  _             = False


pattern CErl_Str       = FCon (UN "CErl_Str")
pattern CErl_Atom      = FCon (UN "CErl_Atom")
pattern CErl_Ptr       = FCon (UN "CErl_Ptr")
pattern CErl_Unit      = FCon (UN "CErl_Unit")

pattern CErl_List a   <- FApp (UN "CErl_List") [_, a]
pattern CErl_Int      <- FApp (UN "CErl_NumT") [_, FCon (UN "CErl_IntNative")]
pattern CErl_Char     <- FApp (UN "CErl_NumT") [_, FCon (UN "CErl_IntChar")]
pattern CErl_Double   <- FApp (UN "CErl_NumT") [_, FCon (UN "CErl_Double")]
pattern CErl_Raw      <- FApp (UN "CErl_Raw")  [_]
pattern CErl_FnT f    <- FApp (UN "CErl_FnT")  [_, f]

pattern CErl_Fn t f   <- FApp (UN "CErl_Fn")     [_, _, t, f]
pattern CErl_FnBase t <- FApp (UN "CErl_FnBase") [_, t]
pattern CErl_FnIO   t <- FApp (UN "CErl_FnIO")   [_, t]


fdesc_to_erlt :: FDesc -> CErlT
fdesc_to_erlt CErl_Str      = CErlString
fdesc_to_erlt CErl_Atom     = CErlAtom
fdesc_to_erlt CErl_Unit     = CErlUnit
fdesc_to_erlt CErl_Ptr      = CErlPtr
fdesc_to_erlt CErl_Raw      = CErlRaw
fdesc_to_erlt CErl_Int      = CErlInt
fdesc_to_erlt CErl_Char     = CErlInt -- We represent chars as integers
fdesc_to_erlt CErl_Double   = CErlDouble
fdesc_to_erlt (CErl_List a) = CErlList (fdesc_to_erlt a)
fdesc_to_erlt (CErl_FnT f)  = fun_fdesc_to_erlt f []
  where
    fun_fdesc_to_erlt (CErl_Fn t g) args =
      fun_fdesc_to_erlt g (fdesc_to_erlt t:args)
    fun_fdesc_to_erlt (CErl_FnBase t) args =
      CErlFn False (fdesc_to_erlt t) (reverse args)
    fun_fdesc_to_erlt (CErl_FnIO t) args =
      CErlFn True (fdesc_to_erlt t) (reverse args)
    fun_fdesc_to_erlt _ _ = CErlRaw
fdesc_to_erlt _ = CErlRaw


check_t :: CErlT -> Exps -> Maybe Exps
check_t CErlString   exp = Just (assert (call "io_lib" "printable_list" [exp]))
check_t CErlAtom     exp = Just (assert (call "erlang" "is_atom"        [exp]))
check_t CErlUnit    _exp = Nothing
check_t CErlPtr     _exp = Nothing
check_t CErlRaw     _exp = Nothing
check_t CErlInt      exp = Just (assert (call "erlang" "is_integer" [exp]))
check_t CErlDouble   exp = Just (assert (call "erlang" "is_float"   [exp]))
check_t (CErlList a) exp = case check_t' a of
  Just fn -> Just (assert (call "lists" "all" [fn, exp]))
  Nothing -> Nothing
check_t _ _ = Nothing

check_t' :: CErlT -> Maybe Exps
{-
check_t' CErlString = Just $
  "fun(_cor0) ->\n    let <_cor1> =\n"
  ++ make_fun "erlang" "is_integer" 1
  ++ "    in  " ++ call "lists" "all" ["_core1", "_cor0"]
check_t' CErlAtom   = Just (make_fun "erlang" "is_atom" 1)
check_t' CErlUnit   = Just $
  "fun(_cor0) ->\n    let <_cor3> =\n" ++ call "erlang" "=:=" ["{}","_cor0"]
  ++ " end"
check_t' CErlInt    = Just (make_fun "erlang" "is_integer" 1)
check_t' CErlDouble = Just (make_fun "erlang" "is_float" 1)
check_t' (CErlList a) = case check_t' a of
  Just fn -> Just $ "fun(_cor0) ->\n    true = lists:all("++ fn ++", L) end"
  Nothing -> Nothing
-}
check_t' _ = Nothing


assert :: Exps -> Exps
assert exp = cerlLet "_cor1" exp body
  where
    body       = constr (Case (constr (S.Var "_cor1"))
                         [Constr trueClause, failClause])
    trueClause = whenTrue (Pats [PLit true']) (Exp (gen (S.Var "_cor1")))
    failClause = gen $ whenTrue (Pats [S.PVar "_cor0"]) match_fail
    whenTrue p = Clause p (Guard (constr (Lit true')))
    true'      = LAtom (Atom "true")
    gen x      = Ann x [CLit (LAtom (Atom "compiler_generated"))]
    match_fail = constr (PrimOp (Atom "match_fail") [constr badmatch])
    badmatch   = Tuple [constr (Lit (LAtom (Atom "badmatch"))),
                        constr (S.Var "_cor0")]
    constr     = Exp . Constr

cerlLet :: VarName -> Exps -> Exps -> Exps
cerlLet var exp body = Exp (Constr (S.Let ([var], exp) body))


--(Exp (Constr (Case (Exp (Constr (Var "_cor1")))
{-
[Constr (Clause (Pats [PLit (LAtom (Atom "true"))])
         (Guard (Exp (Constr (Lit (LAtom (Atom "true"))))))
         (Exp (Ann (Var "_cor1") [CLit (LAtom (Atom "compiler_generated"))]))),
  Ann (Clause (Pats [PVar "_cor0"])
       (Guard (Exp (Constr (Lit (LAtom (Atom "true"))))))
       (Exp (Constr (PrimOp (Atom "match_fail")
                     [Exp (Constr (Tuple [Exp (Constr (Lit (LAtom (Atom "badmatch")))),
                                          Exp (Constr (Var "_cor0"))]))]))))
  [CLit (LAtom (Atom "compiler_generated"))]])))
-}

make_fun :: Text -> Text -> Int -> Exps
make_fun m f a = call "erlang" "make_fun" [m', f', a']
  where m' = Exp (Constr (Lit (LAtom (Atom m))))
        f' = Exp (Constr (Lit (LAtom (Atom f))))
        a' = Exp (Constr (Lit (LInt (toInteger a))))

call :: Text -> Text -> [Exps] -> Exps
call m f a = Exp (Constr (ModCall (m', f') a))
  where m' = Exp (Constr (Lit (LAtom (Atom m))))
        f' = Exp (Constr (FunName (Atom f, toInteger (length a))))

checkedFnCall :: String -> String -> FDesc -> [FDesc] -> String
checkedFnCall nm orig rety args =
  let argtys = map fdesc_to_erlt args
      argNms = argNames argtys
      decl   = fndecl nm argNms
      cbks   = checkedCallbackFuns (zip argtys argNms)
      reschk = checkedResult orig (fdesc_to_erlt rety) (zip argtys argNms)
  in  concat $ [decl] ++ cbks ++ [reschk, "."]

argNames :: [CErlT] -> [String]
argNames = const [""]
{-
argNames tys = let itys = zip tys [1..] in map argName itys
  where argName (ty, ix) = (if cerlFun ty then "CB" else "FC") ++ show ix
-}

fndecl :: String -> [String] -> String
fndecl nm args = nm ++ "(" ++ (", " `intercalate` args) ++ ") ->\n"

checkedCallbackFuns :: [(CErlT,String)] -> [String]
checkedCallbackFuns = mapMaybe (uncurry checkedCallBack)


checkedCallBack :: CErlT -> String -> Maybe String
checkedCallBack _ _ = Nothing
{-
checkedCallBack ty _ | not (cerlFun ty) = Nothing
checkedCallBack (CErlFn unwrap _ret args) nm   = Just $ "Chkd_"++nm++" = fun("++ argstr ++") -> "++ body ++"end,\n"
  where
    args'  = map (\(ty,ix) -> (ty,nm ++ "_" ++ show ix)) (zip args [1..])
    argstr = ", " `intercalate` (map snd args')
    chks   = mapMaybe (uncurry check_t) args'
    finalcall True  xs     = "'APPLY0'("++ finalcall False xs ++", the_world)"
    finalcall False []     = nm
    finalcall False (x:xs) = "'APPLY0'("++ finalcall False xs ++", "++ x ++")"
    body = ",\n" `intercalate` (chks ++ [""++finalcall unwrap (reverse (map snd args'))++""])
-}

checkedResult :: String -> CErlT -> [(CErlT,String)] -> String
checkedResult _ _ _ = ""
{-
checkedResult orig rety args =
  concat $ catMaybes [ Just ("Res = "++ orig ++ "("++ call_args ++"),\n")
                    , fmap (++ ",\n") $ check_t rety "Res"
                    , Just "Res\n"
                    ]
  where
    args'     = map (\(ty,nm) -> if cerlFun ty then "Chkd_" ++ nm else nm) args
    call_args = ", " `intercalate` args'
-}
