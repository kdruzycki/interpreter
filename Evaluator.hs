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
  EApp _ ident exprs -> VStr "TODO" -- to też
  Neg _ expr -> neg expr
  Not _ expr -> not' expr
  EAnd _ expr1 expr2 -> and' expr1 expr2
  EOr _ expr1 expr2 -> or' expr1 expr2
  ERel _ expr1 relop expr2 -> VStr "TODO"
  EMul _ expr1 mulop expr2 -> VStr "TODO"
  EAdd _ expr1 addop expr2 -> VStr "TODO"
  _ -> evalExpr e
  where
    neg :: Expr' a -> Val
    neg e = VInt $ - (fromVInt $ simplifyExpr e)
    not' :: Expr' a -> Val
    not' e = VBool $ not (fromVBool $ simplifyExpr e)
    and' :: Expr' a -> Expr' a -> Val
    and' e1 e2 = VBool $ (fromVBool $ simplifyExpr e1) && (fromVBool $ simplifyExpr e2)
    or' :: Expr' a -> Expr' a -> Val
    or' e1 e2 = VBool $ (fromVBool $ simplifyExpr e1) || (fromVBool $ simplifyExpr e2)

transAddOp :: Show a => AddOp' a -> Result
transAddOp x = case x of
  Plus _ -> failure x
  Minus _ -> failure x

transMulOp :: Show a => MulOp' a -> Result
transMulOp x = case x of
  Times _ -> failure x
  Div _ -> failure x
  Mod _ -> failure x

transRelOp :: Show a => RelOp' a -> Result
transRelOp x = case x of
  LTH _ -> failure x
  LE _ -> failure x
  GTH _ -> failure x
  GE _ -> failure x
  EQU _ -> failure x
  NE _ -> failure x
