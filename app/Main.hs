{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Web.Scotty
import Network.Wai.Middleware.Static hiding ((<|>))
import qualified Data.Map.Strict as M
import Data.Yaml
import System.Random

import Types

main :: IO ()
main = do
  definedCards <- decodeFileThrow "cards.yaml"
  let allCards = definedCards
        `M.union` M.fromList [(i, placeholder i) | i <- map CardId [0..59]]

  vGame <- newEmptyMVar
  _ <- forkIO $ forever $ do
    gen <- newStdGen
    _ <- tryTakeMVar vGame
    putMVar vGame $ initialGame gen
    threadDelay 1000000
  scotty 8880 $ mainApp Env{..}

data Env = Env
  { vGame :: MVar (GameState StdGen)
  , allCards :: M.Map CardId CardInfo
  }

mainApp :: Env -> ScottyM ()
mainApp Env{..} = do
  middleware $ unsafeStaticPolicy (addBase "static")
  get "/" $ file "index.html"
  get "/api/cards" $ json allCards
  get "/api/game" $ do
    gs <- liftIO $ readMVar vGame
    json $ () <$ gs
