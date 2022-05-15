module Statements where

import Globals
import Control.Monad.Reader
import AbsLatteMalinowe

execFnBlock :: Block' a -> [(Ident, Val)] -> ReaderT (FnEnv a) (OutputWriter) ()