module Statements where

import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Writer.Lazy

import Utils
import Globals
import AbsLatteMalinowe
import qualified Evaluator (evalExprM)

type StmtsInterpreter a = StateT VarEnv (ReaderT (FnEnv a) OutputWriter)

execFnBlock :: Block' a -> [(Ident, Val)] -> ReaderT (FnEnv a) OutputWriter Val
execFnBlock b args = do
  retval <- evalStateT (execBlockM b) varEnv
  return $ fromMaybe VVoid retval
  where varEnv = (-1, Map.fromList $ map (mapSnd (\a -> [(0, a)])) args)

execBlockM :: Block' a -> (StmtsInterpreter a) (Maybe Val)
execBlockM (Block _ stmts) = do
  modify $ mapFst (+1)
  ret <- untilJust $ map execStmtM stmts
  lvl <- gets $ fst
  modify $ mapFst (+(-1))
  modify $ mapSnd $ Map.map $ dropLocal lvl
  return ret
    where
      dropLocal :: ScopeLevel -> [(ScopeLevel, Val)] -> [(ScopeLevel, Val)]
      dropLocal scopelvl lvls = case lvls of
        [] -> []
        _ -> if (fst $ head lvls) == scopelvl then tail lvls else lvls

execStmtM :: Stmt' a -> (StmtsInterpreter a) (Maybe Val)
execStmtM s = case s of
  BStmt _ b -> execBlockM b
  Cond _ expr block -> condM expr block
  CondElse _ expr b1 b2 -> condElseM expr b1 b2
  While _ expr block -> whileM expr block
  For _ ident e1 e2 block -> forLoopM ident e1 e2 block
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
      -- Break _ -> failure x
      -- AbsLatteMalinowe.Continue _ -> failure x
      _ -> return ()
    return Nothing

forLoopM :: Ident -> Expr' a -> Expr' a -> Block' a -> (StmtsInterpreter a) (Maybe Val)
forLoopM ident e1 e2 b = do
  v1 <- fmap fromVInt $ evalExprM e1
  v2 <- fmap fromVInt $ evalExprM e2
  scopelvl <- gets $ fst
  forLoopM scopelvl ident v1 v2 (v1 <= v2) b where
    forLoopM :: ScopeLevel -> Ident -> Integer -> Integer -> Bool -> Block' a -> (StmtsInterpreter a) (Maybe Val)
  --   -- trzeba w każdej iteracji zapisywać na nowo, bo blok je kasuje :P
    forLoopM lvl i curr end asc block = do
      if (asc && curr < end || not asc && curr > end)
        then do
          alterLevelsM ident $ (:) (lvl + 1, VInt curr)
          ret <- execBlockM block
          if (isJust ret)
            then (return ret)
            else forLoopM lvl i (if asc then curr + 1 else curr - 1) end asc block
        else return Nothing

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
assM ident e = do
  v <- evalExprM e
  updateVarM ident (const v)

updateVarM :: Ident -> (Val -> Val) -> (StmtsInterpreter a) ()
updateVarM ident f = alterLevelsM ident updateCurrLevelM where
  updateCurrLevelM :: [(ScopeLevel, Val)] -> [(ScopeLevel, Val)]
  updateCurrLevelM (lvl:lvls) = mapSnd f lvl : lvls

alterLevelsM :: Ident -> ([(ScopeLevel, Val)] -> [(ScopeLevel, Val)]) -> (StmtsInterpreter a) ()
alterLevelsM ident modifier = modify $ mapSnd $ Map.alter modifier' ident where
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
  lift $ Evaluator.evalExprM e varEnv
