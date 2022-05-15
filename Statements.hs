module Statements where

import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Writer.Lazy

import Utils
import Globals
import AbsLatteMalinowe
import Evaluator (evalExpr)

type StmtsInterpreter a = StateT VarEnv (ReaderT (FnEnv a) OutputWriter)

execFnBlock :: Block' a -> [(Ident, Val)] -> ReaderT (FnEnv a) OutputWriter Val
execFnBlock b args = do
  retval <- evalStateT (execBlockM b) fnEnv
  return $ fromMaybe VVoid retval
  where fnEnv = (-1, Map.fromList $ map (mapSnd (\a -> [(0, a)])) args)

execBlockM :: Block' a -> (StmtsInterpreter a) (Maybe Val)
execBlockM (Block _ stmts) = do
  modify $ mapFst (+1)
  -- processSeq execStmtM stmts
  ret <- untilJust $ map execStmtM stmts
  lvl <- gets $ fst
  modify $ mapFst (+(-1))
  modify $ mapSnd $ Map.map droplLocal
  return ret
    where
    droplLocal ((lvl, _):lvls) = lvls
    droplLocal lvls = lvls

execStmtM :: Stmt' a -> (StmtsInterpreter a) (Maybe Val)
execStmtM s = case s of
  BStmt _ b -> execBlockM b
  Cond _ expr block -> condM expr block
  CondElse _ expr b1 b2 -> condElseM expr b1 b2
  While _ expr block -> whileM expr block
  Ret _ expr -> evalExprM expr >>= (\v -> return $ return v)
  VRet _ -> return $ return VVoid
  _ -> do
    case s of
      Empty _ -> return ()
      SExp _ e -> (evalExprM e) >> (return ())
      Decl _ type_ items -> processSeq (declVarM type_) items
      Print _ expr -> printM expr
      Ass _ ident expr -> assM ident expr
      Incr _ ident -> updateVarM ident (\v -> VInt $ (fromVInt v) + 1)
      Decr _ ident -> updateVarM ident (\v -> VInt $ (fromVInt v) - 1)
      -- AbsLatteMalinowe.For _ ident expr1 expr2 block -> failure x
      -- AbsLatteMalinowe.Break _ -> failure x
      -- AbsLatteMalinowe.Continue _ -> failure x
      _ -> return ()
    return Nothing
  
-- TODO użycie listen
printM :: Expr' a -> (StmtsInterpreter a) ()
printM e = do
  v <- evalExprM e
  lift $ lift $ tell $ case v of
    VBool _ -> showString $ if (fromVBool v) then "true" else "false"
    VInt _ -> shows $ fromVInt v
    VStr _ -> showString $ fromVStr v
    VVoid -> showString "<void>"

declVarM :: Type' a -> Item' a -> (StmtsInterpreter a) ()
declVarM type_ item = do
  scopelvl <- gets $ fst
  case item of
    NoInit _ ident -> do
      alterLevelsM ident $ (:) (scopelvl, defaultVal type_)
    Init _ ident expr -> do
      v <- evalExprM expr
      alterLevelsM ident $ (:) (scopelvl, v)

assM :: Ident -> Expr' a -> (StmtsInterpreter a) ()
assM i e = do
  v <- evalExprM e
  updateVarM i (const v)

updateVarM :: Ident -> (Val -> Val) -> (StmtsInterpreter a) ()
updateVarM ident f = do
  alterLevelsM ident updateCurrLevelM
  where
    updateCurrLevelM :: [(ScopeLevel, Val)] -> [(ScopeLevel, Val)]
    updateCurrLevelM (lvl:lvls) = mapSnd f lvl : lvls

-- each subscope adds another level of a variable
varLevelsM :: Ident -> (StmtsInterpreter a) [(ScopeLevel, Val)]
varLevelsM ident = gets $ (Map.findWithDefault [] ident) . snd

updateLevelsM :: Ident -> [(ScopeLevel, Val)] -> (StmtsInterpreter a) ()
updateLevelsM ident lvls = modify $ mapSnd $ Map.insert ident lvls

alterLevelsM :: Ident -> ([(ScopeLevel, Val)] -> [(ScopeLevel, Val)]) -> (StmtsInterpreter a) ()
alterLevelsM ident modifier = modify $ mapSnd $ Map.alter modifier' ident
  where
    modifier' :: Maybe [(ScopeLevel, Val)] -> Maybe [(ScopeLevel, Val)]
    modifier' list = return $ modifier $ fromMaybe [] list

condElseM :: Expr' a -> Block' a -> Block' a -> (StmtsInterpreter a) (Maybe Val)
condElseM e b1 b2 = do
  v <- evalExprM e
  if (fromVBool v)
    then execBlockM b1
    else execBlockM b2

condM :: Expr' a -> Block' a -> (StmtsInterpreter a) (Maybe Val)
condM e b = do
  v <- evalExprM e
  if (fromVBool v)
    then execBlockM b
    else return Nothing

-- FIXME handle a return inside a while
whileM :: Expr' a -> Block' a -> (StmtsInterpreter a) (Maybe Val)
whileM e b = do
  v <- evalExprM e
  if (fromVBool v)
    then do
      ret <- execBlockM b
      if (isJust ret) then (return ret) else (whileM e b)
    else
      return Nothing

evalExprM :: Expr' a -> (StmtsInterpreter a) Val
evalExprM e = do
  varEnv <- get
  lift $ evalExpr e varEnv
