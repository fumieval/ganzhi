{-# LANGUAGE ExistentialQuantification #-}
module Types (
  -- * Object identification
  EID(..)
  , Entity(..)
  -- * Automata
  , Component(..)
  , stepComponent
  , bundle
  ) where

import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor
import Barbies

newtype EID = EID { unEID :: Int } deriving (Eq, Ord)

data Entity a = Entity
  { eid :: !EID
  , ebody :: !a
  }

data Component o m i = forall s. Component
  { cState :: s
  , cUpdate :: i -> s -> m s
  , cExpose :: s -> o
  }

stepComponent :: Monad m => i -> Component o m i -> m (Component o m i)
stepComponent i (Component s f e) = f i s <&> \s' -> Component s' f e

bundle :: (Monoid o, TraversableB h, Monad m, ApplicativeB h) => h (Component o m) -> Component o m (h Identity)
bundle cs0 = Component
  { cState = cs0
  , cUpdate = \i cs -> btraverse getCompose $ bzipWith (\(Identity x) -> Compose . stepComponent x) i cs
  , cExpose = bfoldMap $ \(Component s _ e) -> e s
  }
