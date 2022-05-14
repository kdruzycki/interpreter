module Statements where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Writer.Strict

import Utils
import Globals
import AbsLatteMalinowe
import Evaluator (evalExpr)

type StmtsInterpreter a = StateT VarEnv (ReaderT (FnEnv a) (OutputWriter)) ()

execBlock :: Block' a -> VarEnv -> ReaderT (FnEnv a) (OutputWriter) ()
execBlock b varEnv = evalStateT (execBlockM b) varEnv

execBlockM :: Block' a -> StmtsInterpreter a
execBlockM (Block _ stmts) = processSeq execStmtM stmts

execStmtM :: Stmt' a -> StmtsInterpreter a
execStmtM s = case s of
  BStmt _ b -> execBlockM b
  Cond _ e b -> condM e b
  CondElse _ e b1 b2 -> condElseM e b1 b2
  Empty _ -> return ()
  SExp _ e -> do
    varEnv <- get
    v <- lift $ evalExpr e varEnv
    return ()
  Decl _ type_ items -> processSeq (declVarM type_) items
  Print _ expr -> printM expr
  Ass _ ident expr -> assM ident expr
  Incr _ ident -> mapVarIntM ident (+1)
  Decr _ ident -> mapVarIntM ident (+(-1))
  While _ expr block -> whileM expr block
  -- AbsLatteMalinowe.For _ ident expr1 expr2 block -> failure x
  -- AbsLatteMalinowe.Ret _ expr -> failure x
  -- AbsLatteMalinowe.VRet _ -> failure x
  -- AbsLatteMalinowe.Break _ -> failure x
  -- AbsLatteMalinowe.Continue _ -> failure x
  _ -> return ()
  
printM :: Expr' a -> StmtsInterpreter a
printM e = do
  varEnv <- get
  v <- lift $ evalExpr e varEnv
  lift $ lift $ tell $ case v of
    VBool _ -> shows $ fromVBool v
    VInt _ -> shows $ fromVInt v
    VStr _ -> showString $ fromVStr v

declVarM :: Type' a -> Item' a -> StmtsInterpreter a
declVarM type_ item = case item of
  NoInit _ i -> saveVarM i $ defaultVal type_
  Init _ i e -> assM i e

assM :: Ident -> Expr' a -> StmtsInterpreter a
assM i e = do
  varEnv <- get
  v <- lift $ evalExpr e varEnv
  saveVarM i v

mapVarIntM :: Ident -> (Integer -> Integer) -> StmtsInterpreter a
mapVarIntM i f = do
  v <- gets $ Map.findWithDefault (VInt 0) i
  saveVarM i v

saveVarM :: Ident -> Val -> StmtsInterpreter a
saveVarM i v = modify $ Map.insert i v

condElseM :: Expr' a -> Block' a -> Block' a -> StmtsInterpreter a
condElseM e b1 b2 = condDoM e (execBlockM b1) (execBlockM b2)

condM :: Expr' a -> Block' a -> StmtsInterpreter a
condM e b = condDoM e (execBlockM b) (return ())

whileM :: Expr' a -> Block' a -> StmtsInterpreter a
whileM e b = condDoM e (execBlockM b >> whileM e b) (return ())

condDoM :: Expr' a -> StmtsInterpreter a -> StmtsInterpreter a -> StmtsInterpreter a
condDoM e m1 m2 = do
  varEnv <- get
  v <- lift $ evalExpr e varEnv
  if (fromVBool v)
    then m1
    else m2