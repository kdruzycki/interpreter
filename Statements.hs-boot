module Statements where

import Globals
import Control.Monad.Reader
import AbsLatteMalinowe

execBlock :: Block' a -> VarEnv -> ReaderT (FnEnv a) (OutputWriter) ()