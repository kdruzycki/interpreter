module Statements where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Writer.Lazy

import Utils
import Globals
import AbsLatteMalinowe
import Evaluator (evalExpr)

execBlock :: Block' a -> ShowS
execBlock b = (evalState (execWriterT (execBlockM b)) Map.empty)

execBlockM :: Block' a -> WriterT ShowS (State VarEnv) ()
execBlockM (Block _ stmts) = processSeq execStmtM stmts

-- TODO loops

execStmtM :: Stmt' a -> WriterT ShowS (State VarEnv) ()
execStmtM s = case s of
  BStmt _ b -> execBlockM b
  Cond _ e b -> condM e b
  CondElse _ e b1 b2 -> condElseM e b1 b2
  Empty _ -> return ()
  SExp _ e -> (gets $ evalExpr e) >> (return ())
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
  
printM :: Expr' a -> WriterT ShowS (State VarEnv) ()
printM e = do
  v <- gets $ evalExpr e
  tell $ case v of
    VBool _ -> shows $ fromVBool v
    VInt _ -> shows $ fromVInt v
    VStr _ -> showString $ fromVStr v

declVarM :: Type' a -> Item' a -> WriterT ShowS (State VarEnv) ()
declVarM type_ item = case item of
  NoInit _ i -> saveVarM i $ defaultVal type_
  Init _ i e -> assM i e

assM :: Ident -> Expr' a -> WriterT ShowS (State VarEnv) ()
assM i e = do
  v <- gets $ evalExpr e
  saveVarM i v

mapVarIntM :: Ident -> (Integer -> Integer) -> WriterT ShowS (State VarEnv) ()
mapVarIntM i f = do
  v <- gets $ Map.findWithDefault (VInt 0) i
  saveVarM i v

saveVarM :: Ident -> Val -> WriterT ShowS (State VarEnv) ()
saveVarM i v = modify $ Map.insert i v

condElseM :: Expr' a1 -> Block' a2 -> Block' a2 -> WriterT ShowS (State VarEnv) ()
condElseM e b1 b2 = condDoM e (execBlockM b1) (execBlockM b2)

condM :: Expr' a1 -> Block' a2 -> WriterT ShowS (State VarEnv) ()
condM e b = condDoM e (execBlockM b) (return ())

whileM :: Expr' a1 -> Block' a2 -> WriterT ShowS (State VarEnv) ()
whileM e b = condDoM e (execBlockM b >> whileM e b) (return ())

condDoM :: Expr' a1 -> WriterT ShowS (State VarEnv) () -> WriterT ShowS (State VarEnv) () -> WriterT ShowS (State VarEnv) ()
condDoM expr m1 m2 = do
  v <- gets $ evalExpr expr
  if (fromVBool v)
    then m1
    else m2