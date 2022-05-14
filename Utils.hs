module Utils where

processSeq :: (Monoid b, Monad m) => (a -> m b) -> [a] -> m b
processSeq f s = mconcat <$> mapM f s
