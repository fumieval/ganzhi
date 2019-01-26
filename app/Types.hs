{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
module Types where

import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Random

type Effect = ()

data CardInfo = CardInfo
  { cardCost :: !Int
  , cardName :: !Text
  , cardImage :: !Text
  , cardDescription :: !Text
  , cardEffect :: ![Effect]
  } deriving Generic

instance FromJSON CardInfo where
  parseJSON = withObject "CardInfo" $ \v -> CardInfo
    <$> (v .: "cost")
    <*> (v .: "name")
    <*> (v .: "image")
    <*> (v .: "description")
    <*> (v .: "effect")

instance ToJSON CardInfo where
  toJSON CardInfo{..} = object
    [ "cost" .= cardCost
    , "name" .= cardName
    , "image" .= cardImage
    , "description" .= cardDescription
    , "effect" .= cardEffect
    ]

data GameState r = GameState
  { playerState :: !(PlayerState r)
  , enemyState :: !(EnemyState r)
  } deriving (Generic, Functor)

instance FromJSON r => FromJSON (GameState r)
instance ToJSON r => ToJSON (GameState r)

data PlayerState r = PlayerState
  { playerLib :: ![CardId]
  , playerHand :: ![CardId]
  , playerGrave :: ![CardId]
  , playerHealth :: !Int
  , playerBlock :: !Int
  , playerGen :: !r
  } deriving (Generic, Functor)

instance FromJSON r => FromJSON (PlayerState r)
instance ToJSON r => ToJSON (PlayerState r)

data EnemyState r = EnemyState
  { enemyHealth :: !Int
  , enemyBlock :: !Int
  , enemyGen :: !r
  } deriving (Generic, Functor)

instance FromJSON r => FromJSON (EnemyState r)
instance ToJSON r => ToJSON (EnemyState r)

newtype CardId = CardId { unCardId :: Int }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

drawCard :: PlayerState StdGen -> PlayerState StdGen
drawCard ps0 = case splitAt i (playerLib ps0) of
  (xs, y : ys) -> ps0
    { playerLib = xs ++ ys
    , playerHand = y : playerHand ps0
    }
  _ -> ps0
  where
    (i, gen') = randomR (0, length (playerLib ps0) - 1) (playerGen ps0)

initialGame :: StdGen -> GameState StdGen
initialGame gen = GameState
  { playerState = (!!5) $ iterate drawCard $ PlayerState
    { playerLib = map CardId [0..8]
    , playerHand = []
    , playerGrave = []
    , playerHealth = 50
    , playerBlock = 0
    , playerGen = pGen
    }
  , enemyState = EnemyState
    { enemyHealth = 100
    , enemyBlock = 0
    , enemyGen = eGen
    }
  }
  where
    (pGen, eGen) = split gen

placeholder :: CardId -> CardInfo
placeholder (CardId n) = CardInfo
  { cardCost = 0
  , cardName = T.pack
    $ "甲乙丙丁戊己庚辛壬癸" !! mod n 10
    : "子丑寅卯辰巳午未申酉戌亥" !! mod n 12
    : ""
  , cardImage = ""
  , cardDescription = "Lorem ipsum"
  , cardEffect = []
  }
