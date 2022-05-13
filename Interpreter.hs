{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Interpreter where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Writer.Lazy

import Utils
import Globals
import AbsLatteMalinowe
import Statements

interpret :: Show a => Program' a -> Result
interpret = transProgram

transProgram :: Show a => Program' a -> Result
transProgram (Program _ topdefs) = Right VVoid

execProgram :: Show a => Program' a -> Result
execProgram (Program _ topdefs) = Right VVoid

-- transTopDef :: Show a => AbsLatteMalinowe.TopDef' a -> Result
-- transTopDef x = case x of
--   AbsLatteMalinowe.FnDef _ type_ ident args block -> failure x

-- transArg :: Show a => AbsLatteMalinowe.Arg' a -> Result
-- transArg x = case x of
--   AbsLatteMalinowe.Arg _ type_ ident -> failure x

-- transBlock :: Show a => AbsLatteMalinowe.Block' a -> Result
-- transBlock x = case x of
--   AbsLatteMalinowe.Block _ stmts -> failure x