module Statements where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Writer.Lazy

import Utils
import Globals
import AbsLatteMalinowe
import Evaluator (evalExpr)

type StmtsInterpreter a = StateT VarEnv (ReaderT (FnEnv a) (OutputWriter))

execFnBlock :: Block' a -> [(Ident, Val)] -> ReaderT (FnEnv a) (OutputWriter) ()
execFnBlock b args = evalStateT (execBlockM b) fnEnv
  where fnEnv = (-1, Map.fromList $ map (chgSnd (\a -> [(0, a)])) args)

execBlockM :: Block' a -> (StmtsInterpreter a) ()
execBlockM (Block _ stmts) = do
  modify $ chgFst (+1)
  processSeq execStmtM stmts
  lvl <- gets $ fst
  modify $ chgFst (+(-1))
  modify $ chgSnd $ Map.map droplLocal
    where
    droplLocal ((lvl, _):lvls) = lvls
    droplLocal lvls = lvls

execStmtM :: Stmt' a -> (StmtsInterpreter a) ()
execStmtM s = case s of
  BStmt _ b -> execBlockM b
  Cond _ e b -> condM e b
  CondElse _ e b1 b2 -> condElseM e b1 b2
  Empty _ -> return ()
  SExp _ e -> (evalExprM e) >> (return ())
  Decl _ type_ items -> processSeq (declVarM type_) items
  Print _ expr -> printM expr
  Ass _ ident expr -> assM ident expr
  Incr _ ident -> updateVarM ident (\v -> VInt $ (fromVInt v) + 1)
  Decr _ ident -> updateVarM ident (\v -> VInt $ (fromVInt v) - 1)
  While _ expr block -> whileM expr block
  -- AbsLatteMalinowe.For _ ident expr1 expr2 block -> failure x
  -- AbsLatteMalinowe.Ret _ expr -> failure x
  -- AbsLatteMalinowe.VRet _ -> failure x
  -- AbsLatteMalinowe.Break _ -> failure x
  -- AbsLatteMalinowe.Continue _ -> failure x
  _ -> return ()
  
printM :: Expr' a -> (StmtsInterpreter a) ()
printM e = do
  v <- evalExprM e
  lift $ lift $ tell $ case v of
    VBool _ -> shows $ fromVBool v
    VInt _ -> shows $ fromVInt v
    VStr _ -> showString $ fromVStr v

declVarM :: Type' a -> Item' a -> (StmtsInterpreter a) ()
declVarM type_ item = do
  scopelvl <- gets $ fst
  case item of
    NoInit _ ident -> do
      varlevels <- varLevelsM ident
      updateLevelsM ident $ (scopelvl, defaultVal type_) : varlevels
    Init _ ident expr -> do
      varlevels <- varLevelsM ident
      v <- evalExprM expr
      updateLevelsM ident $ (scopelvl, v) : varlevels

assM :: Ident -> Expr' a -> (StmtsInterpreter a) ()
assM i e = do
  v <- evalExprM e
  updateVarM i (const v)

updateVarM :: Ident -> (Val -> Val) -> (StmtsInterpreter a) ()
updateVarM ident f = do
  varlevels <- gets $ (Map.lookup ident) . snd
  mapM_ updateCurrLevelM varlevels where
    updateCurrLevelM :: [(ScopeLevel, Val)] -> (StmtsInterpreter a) ()
    updateCurrLevelM (lvl:lvls) = updateLevelsM ident $ chgSnd f lvl : lvls

-- each subscope adds another level of a variable
varLevelsM :: Ident -> (StmtsInterpreter a) [(ScopeLevel, Val)]
varLevelsM ident = gets $ (Map.findWithDefault [] ident) . snd

updateLevelsM :: Ident -> [(ScopeLevel, Val)] -> (StmtsInterpreter a) ()
updateLevelsM ident lvls = modify $ chgSnd $ Map.insert ident lvls

condElseM :: Expr' a -> Block' a -> Block' a -> (StmtsInterpreter a) ()
condElseM e b1 b2 = condDoM e (execBlockM b1) (execBlockM b2)

condM :: Expr' a -> Block' a -> (StmtsInterpreter a) ()
condM e b = condDoM e (execBlockM b) (return ())

whileM :: Expr' a -> Block' a -> (StmtsInterpreter a) ()
whileM e b = condDoM e (execBlockM b >> whileM e b) (return ())

condDoM :: Expr' a -> (StmtsInterpreter a) () -> (StmtsInterpreter a) () -> (StmtsInterpreter a) ()
condDoM e m1 m2 = do
  v <- evalExprM e
  if (fromVBool v)
    then m1
    else m2

evalExprM :: Expr' a -> (StmtsInterpreter a) Val
evalExprM e = do
  varEnv <- get
  lift $ evalExpr e varEnv