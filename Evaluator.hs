module Evaluator where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Prelude

import Globals
import AbsLatteMalinowe

-- TODO zwracanie Result, zamiast Val, żeby można było dać ulubiony błąd wszystkich, czyli dzielenie przez zero

evalExpr :: Expr' a -> Val
evalExpr e = case e of
  ELitInt _ n -> VInt n
  ELitTrue _ -> VBool True
  ELitFalse _ -> VBool False
  EString _ s -> VStr s
  _ -> simplifyExpr e

simplifyExpr :: Expr' a -> Val
simplifyExpr e = case e of
  EVar _ ident -> VStr "TODO" -- to zróbmy w oddzielnym module (variables manipulation)
  EApp _ ident es -> VStr "TODO" -- to w module dot. funkcji
  Neg _ e -> neg $ simplifyExpr e
  Not _ e -> not' $ simplifyExpr e
  EAnd _ e1 e2 -> and' (simplifyExpr e1) (simplifyExpr e2)
  EOr _ e1 e2 -> or' (simplifyExpr e1) (simplifyExpr e2)
  ERel _ e1 op e2 -> erel op (simplifyExpr e1) (simplifyExpr e2)
  EMul _ e1 op e2 -> emul op (simplifyExpr e1) (simplifyExpr e2)
  EAdd _ e1 op e2 -> eadd op (simplifyExpr e1) (simplifyExpr e2)
  _ -> evalExpr e
    
neg :: Val -> Val
neg v = VInt $ - (fromVInt v)

not' :: Val -> Val
not' v = VBool $ not (fromVBool v)

and' :: Val -> Val -> Val
and' v1 v2 = VBool $ (fromVBool v1) && (fromVBool v2)

or' :: Val -> Val -> Val
or' v1 v2 = VBool $ (fromVBool v1) || (fromVBool v2)

eadd :: AddOp' a -> Val -> Val -> Val
eadd op v1 v2 = case op of
  Minus _ -> VInt $ (fromVInt v1) - (fromVInt v2)
  Plus _ -> case v1 of
    VStr _ -> VStr $ showString (fromVStr v1) (fromVStr v2)
    VInt _ -> VInt $ (fromVInt v1) + (fromVInt v2)

emul :: MulOp' a -> Val -> Val -> Val
emul op v1 v2 = case op of
  Div _ -> VInt $ (fromVInt v1) `div` (fromVInt v2)
  Mod _ -> VInt $ (fromVInt v1) `mod` (fromVInt v2)
  Times _ -> case v1 of
    VStr _ -> VStr $ concat $ replicate (fromIntegral $ fromVInt v2) (fromVStr v1)
    VInt _ -> case v2 of
      VStr _ -> VStr $ concat $ replicate (fromIntegral $ fromVInt v1) (fromVStr v2)
      VInt _ -> VInt $ (fromVInt v1) * (fromVInt v2)

erel :: RelOp' a -> Val -> Val -> Val
erel op v1 v2 = case v1 of
  VBool _ -> VBool $ (transRelOp op) (fromVBool v1) (fromVBool v2)
  VStr  _ -> VBool $ (transRelOp op) (fromVStr v1)  (fromVStr v2)
  VInt  _ -> VBool $ (transRelOp op) (fromVInt v1)  (fromVInt v2)
  where
    transRelOp :: Ord a1 => RelOp' a2 -> a1 -> a1 -> Bool
    transRelOp op = case op of
      NE  _ -> (/=)
      EQU _ -> (==)
      LE  _ -> (<=)
      GE  _ -> (>=)
      GTH _ -> (>)
      LTH _ -> (<)