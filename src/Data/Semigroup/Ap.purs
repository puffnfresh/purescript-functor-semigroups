module Data.Semigroup.Ap where

import Data.Monoid (class Monoid, mempty)
import Prelude (class Applicative, class Apply, class Functor, class Semigroup, ($), (<$>), (<*>), apply, append, pure)

newtype Ap m a = Ap (m a)

runAp :: forall m a. Ap m a -> m a
runAp (Ap ma) = ma

instance apSemigroup :: (Apply m, Semigroup a) => Semigroup (Ap m a) where
  append (Ap a) (Ap b) = Ap $ append <$> a <*> b

instance apMonoid :: (Applicative m, Monoid a) => Monoid (Ap m a) where
  mempty = Ap $ pure mempty

instance apFunctor :: (Functor m) => Functor (Ap m) where
  map f (Ap ma) = Ap $ f <$> ma

instance apApply :: (Apply m) => Apply (Ap m) where
  apply (Ap f) (Ap a) = Ap $ apply f a

instance apApplicative :: (Applicative m) => Applicative (Ap m) where
  pure a = Ap $ pure a
