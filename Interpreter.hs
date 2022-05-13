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
execProgram p = print $ execState (execProgramM p) Map.empty

execProgramM :: Program' a -> State (FnEnv a) ()
execProgramM (Program _ fs) = do
  processSeq saveFnM fs
  main <- gets $ Map.lookup (Ident "main")
  mapM_ execFnM main
  -- case main of
  --   Just fn -> execFnM fn

saveFnM :: TopDef' a -> State (FnEnv a) ()
saveFnM (FnDef _ _ ident args block) = 
  modify $ Map.insert ident (map argIdent args, block)
  where
    argIdent :: Arg' a -> Ident
    argIdent (Arg _ _ ident) = ident

execFnM :: FnSgn a -> State (FnEnv a) ()
execFnM (argIdents, block) = return ()

--execBlock :: Block' a -> IO ()
--execBlock b = putStr $ (evalState (execWriterT (execBlockM b)) Map.empty) "\n"

--execBlockM :: Block' a -> WriterT ShowS (State IdentEnv) ()
--execBlockM (Block _ stmts) = processSeq execStmtM stmts


-- transTopDef :: Show a => AbsLatteMalinowe.TopDef' a -> Result
-- transTopDef x = case x of
--   AbsLatteMalinowe.FnDef _ type_ ident args block -> failure x

-- transArg :: Show a => AbsLatteMalinowe.Arg' a -> Result
-- transArg x = case x of
--   AbsLatteMalinowe.Arg _ type_ ident -> failure x

-- transBlock :: Show a => AbsLatteMalinowe.Block' a -> Result
-- transBlock x = case x of
--   AbsLatteMalinowe.Block _ stmts -> failure x