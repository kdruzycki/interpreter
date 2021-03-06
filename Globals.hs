module Globals where

import qualified Data.Map as Map
import Control.Monad.Trans.Writer.Lazy

import AbsLatteMalinowe

data Val
  = VInt  Integer
  | VStr  String
  | VBool Bool
  | VVoid
  | VBrk | VCnt
  deriving (Show, Eq)

-- pamietamy tylko identyfikatory, nie pamiętamy typów, bo typechecker
type FnSgn a = ([Ident], Block' a)
type FnEnv a = Map.Map Ident (FnSgn a)

type ScopeLevel = Int
type VarEnv = (ScopeLevel, Map.Map Ident [(ScopeLevel, Val)])
type OutputWriter = Writer ShowS

defaultVal :: Type' a -> Val
defaultVal t = case t of
  Int _ -> VInt 0
  Bool _ -> VBool False
  Str _ -> VStr ""

fromVInt :: Val -> Integer
fromVInt (VInt v) = v

fromVBool :: Val -> Bool
fromVBool (VBool v) = v

fromVStr :: Val -> String
fromVStr (VStr v) = v

mapVInt :: (Integer -> Integer) -> Val -> Val
mapVInt f (VInt i) = VInt $ f i

mapVStr :: (String -> String) -> Val -> Val
mapVStr f (VStr s) = VStr $ f s

mapVBool :: (Bool -> Bool) -> Val -> Val
mapVBool f (VBool b) = VBool $ f b