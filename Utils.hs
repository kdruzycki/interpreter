module Utils where

import Data.Maybe

processSeq :: (Monoid b, Monad m) => (a -> m b) -> [a] -> m b
processSeq f s = mconcat <$> mapM f s

untilJust :: Monad m => [m (Maybe a)] -> m (Maybe a)
untilJust [] = return Nothing
untilJust (m:ms) = do
  maybeV <- m
  if (isJust maybeV)
    then return maybeV
    else untilJust ms

mapSnd :: (t -> b) -> (a, t) -> (a, b)
mapSnd f (x, y) = (x, f y)

mapFst :: (t -> b) -> (t, a) -> (b, a)
mapFst f (x, y) = (f x, y)