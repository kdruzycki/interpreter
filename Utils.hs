module Utils where

processSeq :: (Monoid b, Monad m) => (a -> m b) -> [a] -> m b
processSeq f s = mconcat <$> mapM f s

mapSnd :: (t -> b) -> (a, t) -> (a, b)
mapSnd f (x, y) = (x, f y)

mapFst :: (t -> b) -> (t, a) -> (b, a)
mapFst f (x, y) = (f x, y)