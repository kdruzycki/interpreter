module UtilsLatteMalinowe where

import AbsLatteMalinowe

processSeq :: (Monoid b, Monad m) => (a -> m b) -> [a] -> m b
processSeq f s = mconcat <$> mapM f s

-- disclaimer nie trzeba sprawdzać typu zmiennej i wartości, bo typechecker
valueItem :: Type' a -> Item' a -> Val
valueItem t i = case i of
  NoInit _ _ -> case t of
    Int _ -> VInt 0
    Bool _ -> VBool False
    Str _ -> VStr ""
  -- Init _ _ expr -> expr
  Init _ _ expr -> VInt 0

identItem :: Item' a -> Ident
identItem i = case i of
  NoInit _ ident -> ident
  Init _ ident _ -> ident