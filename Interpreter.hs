{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Interpreter where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Writer.Lazy

import Utils
import Globals
import AbsLatteMalinowe
import Statements

type FnsInterpreter a = StateT (FnEnv a) (OutputWriter) ()

execProgram :: Program' a -> Either String String
execProgram p = Right $ (execWriter (evalStateT (execProgramM p) Map.empty)) "\n"

execProgramM :: Program' a -> FnsInterpreter a
execProgramM (Program _ fs) = do
  processSeq saveFnM fs
  main <- gets $ Map.lookup (Ident "main")
  mapM_ execFnM main

saveFnM :: TopDef' a -> FnsInterpreter a
saveFnM (FnDef _ _ ident args block) = 
  modify $ Map.insert ident (map argIdent args, block)
  where
    argIdent :: Arg' a -> Ident
    argIdent (Arg _ _ ident) = ident

execFnM :: FnSgn a -> FnsInterpreter a
execFnM (argIdents, b) = lift $ execBlock b
