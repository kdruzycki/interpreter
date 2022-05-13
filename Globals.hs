module Globals where

import qualified Data.Map as Map

import AbsLatteMalinowe

data Val
  = VInt  Integer
  | VStr  String
  | VBool Bool
  | VVoid
  deriving (Show)
  
-- pamietamy tylko identyfikatory, nie pamiętamy typów, bo typechecker
type VarEnv = Map.Map Ident Val
type FnSgn a = ([Ident], Block' a)
type FnEnv a = Map.Map Ident (FnSgn a)

type Err = Either String
type Result = Err Val

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

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