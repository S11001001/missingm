module Control.Monad.MissingM where

import Prelude hiding (foldr)
import Control.Arrow (first)
import Control.Monad (liftM)
import Data.List (tails)
import Data.Maybe (isJust)
import Data.Foldable (Foldable, foldr)

-- | Monadic branching.  Unlike liftA3 if', only one of the actions is
-- run; hence, the applicative instance has different behavior.
ifM :: Monad m => m Bool
       -> m a                   -- ^ If the bool is true.
       -> m a                   -- ^ If the bool is false.
       -> m a
ifM t c a = do
  b <- t
  if b then c else a

-- | Monadic search, left to right, stopping when an element yielding
-- True is found.
findM ::
  (Foldable t, Monad m) =>
  (a -> m Bool)                 -- ^ Monadic test.
  -> t a                        -- ^ Search space.
  -> m (Maybe a)
findM f = foldr rec (return Nothing)
  where rec x = ifM (f x) (return . Just $ x)

-- | Monadic search and collection of the first result, searching left
-- to right.
findMapM ::
  (Foldable t, Monad m) =>
  (a -> m (Maybe b))
  -> t a
  -> m (Maybe b)
findMapM f = foldr rec (return Nothing)
  where rec x r = do
          mb <- f x
          if isJust mb
            then return mb
            else r

-- | Take until the first non-Just result.
spanMapM ::
  Monad m =>
  (a -> m (Maybe b))
  -> [a]
  -> m ([b], [a])
spanMapM f = foldr rec empty . tails
  where empty = return ([], [])
        rec [] _ = empty
        rec xs@(x:_) mx = f x >>=
          maybe (return ([], xs)) (\b -> first (b:) `liftM` mx)
