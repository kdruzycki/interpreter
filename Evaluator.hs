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
  EVar _ ident -> VStr "TODO"
  EApp _ ident exprs -> VStr "TODO"
  Neg _ expr -> VStr "TODO"
  Not _ expr -> VStr "TODO"
  EMul _ expr1 mulop expr2 -> VStr "TODO"
  EAdd _ expr1 addop expr2 -> VStr "TODO"
  ERel _ expr1 relop expr2 -> VStr "TODO"
  EAnd _ expr1 expr2 -> VStr "TODO"
  EOr _ expr1 expr2 -> VStr "TODO"

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
