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

interpret :: Show a => Program' a -> Result
interpret = transProgram

transProgram :: Show a => Program' a -> Result
transProgram _ = Right VVoid

execProgram :: Show a => Program' a -> IO ()
execProgram p = putStr $ (evalState (execWriterT (execProgramM p)) Map.empty) "\n"

execProgramM :: Program' a -> WriterT ShowS (State (FnEnv a)) ()
execProgramM (Program _ fs) = do
  processSeq saveFnM fs
  main <- gets $ Map.lookup (Ident "main")
  mapM_ execFnM main

saveFnM :: TopDef' a -> WriterT ShowS (State (FnEnv a)) ()
saveFnM (FnDef _ _ ident args block) = 
  modify $ Map.insert ident (map argIdent args, block)
  where
    argIdent :: Arg' a -> Ident
    argIdent (Arg _ _ ident) = ident

execFnM :: FnSgn a -> WriterT ShowS (State (FnEnv a)) ()
execFnM (argIdents, b) = tell $ execBlock b
