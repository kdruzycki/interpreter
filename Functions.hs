{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Functions where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Writer.Strict

import Utils
import Globals
import AbsLatteMalinowe
import {-# SOURCE #-} Statements (execBlock)

-- TODO
-- 1. Zamienić State na Writer w tym module
-- 2. Przekazywać fnEnv do Statements i zrobić tam reader tego
-- 3. Zrobić reader fnEnv w Ewaluatorze i w ten sposób wykonać tam execFnM

-- pamietamy tylko identyfikatory, nie pamiętamy typów, bo typechecker
type FnSgn a = ([Ident], Block' a)
type FnEnv a = Map.Map Ident (FnSgn a)
type FnsInterpreter a = StateT (FnEnv a) (OutputWriter) ()

execProgram :: Program' a -> Either String String
execProgram p = Right $ (execWriter (evalStateT (execProgramM p) Map.empty)) "\n"

-- z tego robimy writer do fnEnva (czyli WriterT (FnEnv a) (OutuptWriter) ())
execProgramM :: Program' a -> FnsInterpreter a
execProgramM (Program _ fs) = do
  processSeq saveFnM fs
  main <- gets $ Map.lookup (Ident "main")
  mapM_ (\fnSgn -> execFnM fnSgn []) main

-- z tego też
saveFnM :: TopDef' a -> FnsInterpreter a
saveFnM (FnDef _ _ ident args block) = 
  modify $ Map.insert ident (map argIdent args, block)
  where
    argIdent :: Arg' a -> Ident
    argIdent (Arg _ _ ident) = ident

-- z tego robimy reader FnEnva
execFnM :: FnSgn a -> [Val] -> FnsInterpreter a
execFnM (argIdents, b) argVals = lift $ execBlock b varEnv
  where varEnv = Map.fromList $ zip argIdents argVals
