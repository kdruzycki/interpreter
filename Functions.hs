{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Functions where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Writer.Strict

import Utils
import Globals
import AbsLatteMalinowe
import {-# SOURCE #-} Statements (execBlock)

-- TODO
-- 3. Zrobić reader fnEnv w Ewaluatorze i w ten sposób wykonać tam execFnM

programOutput :: Program' a -> Either String String
programOutput p = return $ (execWriter $ execProgram p) "\n"

execProgram :: Program' a -> OutputWriter ()
execProgram p = runReaderT execProgramM (createFnEnv p)

execProgramM :: ReaderT (FnEnv a) OutputWriter ()
execProgramM = do
  main <- asks $ Map.lookup (Ident "main")
  mapM_ (\fnSgn -> runFnM fnSgn []) main

runFnM :: FnSgn a -> [Val] -> ReaderT (FnEnv a) OutputWriter ()
runFnM (argIdents, b) argVals = execBlock b varEnv
  where varEnv = Map.fromList $ zip argIdents argVals

createFnEnv :: Program' a -> FnEnv a
createFnEnv (Program _ fnDefs) = Map.fromList $ map fnEnvEntry fnDefs
  where
    fnEnvEntry :: TopDef' a -> (Ident, (FnSgn a))
    fnEnvEntry (FnDef _ _ ident args block) = 
      (ident, (map argIdent args, block))
      where
        argIdent :: Arg' a -> Ident
        argIdent (Arg _ _ ident) = ident
