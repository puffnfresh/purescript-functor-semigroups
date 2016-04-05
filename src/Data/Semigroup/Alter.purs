module Data.Semigroup.Alter where

import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus, empty)
import Data.Monoid (class Monoid)
import Prelude (class Functor, class Semigroup, ($), (<$>))

newtype Alter m a = Alter (m a)

runAlter :: forall m a. Alter m a -> m a
runAlter (Alter ma) = ma

instance alterSemigroup :: (Alt m) => Semigroup (Alter m a) where
  append (Alter a) (Alter b) = Alter $ a <|> b

instance alterMonoid :: (Plus m) => Monoid (Alter m a) where
  mempty = Alter empty

instance alterFunctor :: (Functor m) => Functor (Alter m) where
  map f (Alter ma) = Alter $ f <$> ma

instance alterAlt :: (Alt m) => Alt (Alter m) where
  alt (Alter a) (Alter b) = Alter $ a <|> b

instance alterPlus :: (Plus m) => Plus (Alter m) where
  empty = Alter empty
