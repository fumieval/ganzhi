{-# LANGUAGE StrictData #-}
module Model where

import Types

newtype Target = MkTarget Int

data Effect = EDamage Target Int
  | EDraw Int
  | EThen Effect Effect
  | ETarget Effect
  | ENone

instance Semigroup Effect where
  (<>) = EThen

data Combat h = Combat
  { combatHand :: h [Entity Card]
  }

data Card = Card
  { cardName :: String
  , cardCost :: Int
  , cardEffect :: Effect
  }
