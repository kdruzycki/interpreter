module Globals where

import qualified Data.Map as Map

import AbsLatteMalinowe

data Val
  = VInt  Integer
  | VStr  String
  | VBool Bool
  | VVoid
  deriving (Show)
  -- todo funkcje jako val

type IdentEnv = Map.Map Ident Val

type Err = Either String
type Result = Err Val

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

defaultVal :: Type' a -> Val
defaultVal t = case t of
  Int _ -> VInt 0
  Bool _ -> VBool False
  Str _ -> VStr ""