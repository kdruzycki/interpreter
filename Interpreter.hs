{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Interpreter where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Writer.Lazy

import Utils
import Globals
import Evaluator (evalExpr)
import AbsLatteMalinowe

interpret :: Show a => Program' a -> Result
interpret = transProgram

transProgram :: Show a => Program' a -> Result
transProgram x = case x of
  AbsLatteMalinowe.Program _ topdefs -> Right VVoid

-- transTopDef :: Show a => AbsLatteMalinowe.TopDef' a -> Result
-- transTopDef x = case x of
--   AbsLatteMalinowe.FnDef _ type_ ident args block -> failure x

-- transArg :: Show a => AbsLatteMalinowe.Arg' a -> Result
-- transArg x = case x of
--   AbsLatteMalinowe.Arg _ type_ ident -> failure x

-- transBlock :: Show a => AbsLatteMalinowe.Block' a -> Result
-- transBlock x = case x of
--   AbsLatteMalinowe.Block _ stmts -> failure x

execBlock :: Block' a -> IO ()
execBlock b = putStr $ (evalState (execWriterT (execBlockM b)) Map.empty) "\n"

execBlockM :: Block' a -> WriterT ShowS (State IdentEnv) ()
execBlockM (Block _ stmts) = processSeq execStmtM stmts

-- TODO loops

execStmtM :: Stmt' a -> WriterT ShowS (State IdentEnv) ()
execStmtM s = case s of
  BStmt _ b -> execBlockM b
  Cond _ e b -> do
    v <- gets $ evalExpr e
    when (fromVBool v) $ execBlockM b
  CondElse _ e b1 b2-> do
    v <- gets $ evalExpr e
    if (fromVBool v)
      then execBlockM b1
      else execBlockM b2
  Empty _ -> return ()
  SExp _ e -> (gets $ evalExpr e) >> (return ())
  Decl _ type_ items -> processSeq (declVarM type_) items
  Print _ expr -> printM expr
  Ass _ ident expr -> assM ident expr
  Incr _ ident -> mapVarIntM ident (+1)
  Decr _ ident -> mapVarIntM ident (+(-1))
  _ -> return ()
  -- AbsLatteMalinowe.While _ expr lblock -> failure x
  -- AbsLatteMalinowe.For _ ident expr1 expr2 lblock -> failure x
  -- AbsLatteMalinowe.Ret _ expr -> failure x
  -- AbsLatteMalinowe.VRet _ -> failure x
  -- AbsLatteMalinowe.Break _ -> failure x
  -- AbsLatteMalinowe.Continue _ -> failure x
  
printM :: Expr' a -> WriterT ShowS (State IdentEnv) ()
printM e = do
  v <- gets $ evalExpr e
  tell $ case v of
    VBool _ -> shows $ fromVBool v
    VInt _ -> shows $ fromVInt v
    VStr _ -> showString $ fromVStr v

declVarM :: Type' a -> Item' a -> WriterT ShowS (State IdentEnv) ()
declVarM type_ item = case item of
  NoInit _ i -> saveVarM i $ defaultVal type_
  Init _ i e -> assM i e

assM :: Ident -> Expr' a -> WriterT ShowS (State IdentEnv) ()
assM i e = do
  v <- gets $ evalExpr e
  saveVarM i v

mapVarIntM :: Ident -> (Integer -> Integer) -> WriterT ShowS (State IdentEnv) ()
mapVarIntM i f = do
  v <- gets $ Map.findWithDefault (VInt 0) i
  saveVarM i v

saveVarM :: Ident -> Val -> WriterT ShowS (State IdentEnv) ()
saveVarM i v = modify $ Map.insert i v
