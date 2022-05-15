module Evaluator where

import qualified Data.Map as Map
import Control.Monad.Reader
import Prelude hiding (EQ, GT, LT)

import Globals
import Functions (runFnM)
import AbsLatteMalinowe

evalExpr :: Expr' a -> VarEnv -> ReaderT (FnEnv a) OutputWriter Val
evalExpr e varEnv = runReaderT (eval e) varEnv

eval :: Expr' a -> ReaderT VarEnv (ReaderT (FnEnv a) OutputWriter) Val
eval e = case e of
  LitInt _ n -> return $ VInt n
  LitTrue _ -> return $ VBool True
  LitFalse _ -> return $ VBool False
  LitString _ s -> return $ VStr s
  Var _ ident -> var ident 
  App _ ident es -> do
    args <- mapM eval es
    lift $ app ident args
  Neg _ e -> neg <$> (eval e)
  Not _ e -> not' <$> (eval e)
  And _ e1 e2 -> and' <$> (eval e1) <*> (eval e2)
  Or  _ e1 e2 -> or' <$> (eval e1) <*> (eval e2)
  Rel _ e1 op e2 -> rel op <$> (eval e1) <*> (eval e2)
  Mul _ e1 op e2 -> mul op <$> (eval e1) <*> (eval e2)
  Add _ e1 op e2 -> add op <$> (eval e1) <*> (eval e2)

var :: Ident -> ReaderT VarEnv (ReaderT (FnEnv a) OutputWriter) Val
var ident = do
  varlevels <- asks $ (Map.lookup ident) . snd
  case varlevels of
    Just lvls -> return $ snd $ head $ lvls
    Nothing -> return $ VBool False

app :: Ident -> [Val] -> ReaderT (FnEnv a) OutputWriter Val
app ident args = do
  maybeFn <- asks $ Map.lookup ident
  mapM (\f -> runFnM f args) maybeFn
  return $ VStr "Returning functions TODO"

neg :: Val -> Val
neg v = VInt $ - (fromVInt v)

not' :: Val -> Val
not' v = VBool $ not (fromVBool v)

and' :: Val -> Val -> Val
and' v1 v2 = VBool $ (fromVBool v1) && (fromVBool v2)

or' :: Val -> Val -> Val
or' v1 v2 = VBool $ (fromVBool v1) || (fromVBool v2)

add :: AddOp' a -> Val -> Val -> Val
add op v1 v2 = case op of
  Minus _ -> VInt $ (fromVInt v1) - (fromVInt v2)
  Plus _ -> case v1 of
    VStr _ -> VStr $ showString (fromVStr v1) (fromVStr v2)
    VInt _ -> VInt $ (fromVInt v1) + (fromVInt v2)

mul :: MulOp' a -> Val -> Val -> Val
mul op v1 v2 = case op of
  Div _ -> VInt $ (fromVInt v1) `div` (fromVInt v2)
  Mod _ -> VInt $ (fromVInt v1) `mod` (fromVInt v2)
  Times _ -> case v1 of
    VStr _ -> VStr $ concat $ replicate (fromIntegral $ fromVInt v2) (fromVStr v1)
    VInt _ -> case v2 of
      VStr _ -> VStr $ concat $ replicate (fromIntegral $ fromVInt v1) (fromVStr v2)
      VInt _ -> VInt $ (fromVInt v1) * (fromVInt v2)

rel :: RelOp' a -> Val -> Val -> Val
rel op v1 v2 = case v1 of
  VBool _ -> VBool $ (transRelOp op) (fromVBool v1) (fromVBool v2)
  VStr  _ -> VBool $ (transRelOp op) (fromVStr v1)  (fromVStr v2)
  VInt  _ -> VBool $ (transRelOp op) (fromVInt v1)  (fromVInt v2)
  where
    transRelOp :: Ord a1 => RelOp' a2 -> a1 -> a1 -> Bool
    transRelOp op = case op of
      NEQ _ -> (/=)
      EQU _ -> (==)
      LTE _ -> (<=)
      GTE _ -> (>=)
      GTH _ -> (>)
      LTH _ -> (<)
