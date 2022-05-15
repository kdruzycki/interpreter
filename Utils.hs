module Utils where

processSeq :: (Monoid b, Monad m) => (a -> m b) -> [a] -> m b
processSeq f s = mconcat <$> mapM f s

chgSnd :: (t -> b) -> (a, t) -> (a, b)
chgSnd f (x, y) = (x, f y)

chgFst :: (t -> b) -> (t, a) -> (b, a)
chgFst f (x, y) = (f x, y)