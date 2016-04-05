module Data.Semigroup.Traversal where

import Control.Apply ((*>))
import Data.Monoid (class Monoid)
import Prelude (class Applicative, class Apply, class Semigroup, Unit, ($), pure, unit)

newtype Traversal m = Traversal (m Unit)

runTraversal :: forall m. Traversal m -> m Unit
runTraversal (Traversal m) = m

instance traversalSemigroup :: (Apply m) => Semigroup (Traversal m) where
  append (Traversal a) (Traversal b) = Traversal $ a *> b

instance traversalMonoid :: (Applicative m) => Monoid (Traversal m) where
  mempty = Traversal $ pure unit
