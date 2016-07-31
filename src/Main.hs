module Main where

import           Idris.AbsSyntax        (Codegen (Via), IRFormat (IBCFormat),
                                         Idris, runIO)
-- import           Idris.Core.TT
import           Idris.ElabDecls        (elabMain, elabPrims)
import           Idris.REPL             (loadInputs, runMain)

import           IRTS.CodegenCoreErlang (codegenCoreErlang)
import           IRTS.Compiler          (compile)

import           Control.Monad          (liftM)

import           System.Environment     (getArgs)
import           System.Exit            (ExitCode (ExitSuccess), exitWith)

import           Paths_idris_cerl       (getDataFileName)

data Opts = Opts
  { inputs    :: [FilePath]
  , output    :: FilePath
  , show_path :: Bool
  , interface :: Bool
  }

erlDefaultOpts :: Opts
erlDefaultOpts = Opts
  { inputs    = []
  , output    = "main.erl"
  , show_path = False
  , interface = False
  }

showUsage :: IO ()
showUsage = do
  putStrLn "Usage: idris-cerl [--interface] [--path] <ibc-files> [-o <output-file>]"
  exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do
  xs <- getArgs
  return $ process erlDefaultOpts xs
  where
    process opts ("--interface":xs) = process (opts { interface = True }) xs
    process opts ("--path":_) = opts {show_path = True}
    process opts ("-o":o:xs)  = process (opts {output = o}) xs
    process opts (x:xs)       = process (opts {inputs = x:inputs opts}) xs
    process opts []           = opts

erl_main :: Opts -> Idris ()
erl_main opts = do
  elabPrims
  _        <- loadInputs (inputs opts) Nothing
  mainProg <- if interface opts then return Nothing else liftM Just elabMain
  ir       <- compile (Via IBCFormat "cerl") (output opts) mainProg
  runIO $ codegenCoreErlang ir

main :: IO ()
main = do
  opts     <- getOpts
  data_dir <- getDataFileName "irts"
  if show_path opts
    then putStrLn ("-pa " ++ data_dir ++ "") >> exitWith ExitSuccess
    else return ()
  if null (inputs opts)
    then showUsage
    else runMain (erl_main opts)
