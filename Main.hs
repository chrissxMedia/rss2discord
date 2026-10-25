{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, catchAny, displayException, fromException)
import Control.Monad (forM_)
import Feed (pollFeed)
import FeedConfig (FeedConfig (..), readFeeds)
import History (History, withHistory)
import Network.HTTP.Client (HttpException (..), Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)

main :: IO ()
main = run `catchAny` \e -> die ("rss2discord failed. " <> errorMessage e)
  where
    run = do
      feeds <- readFeeds "feeds.csv"
      createDirectoryIfMissing True "/data"
      manager <- newTlsManager
      withHistory "/data/history.sqlite3" $ \history -> loop feeds manager history

    loop :: [FeedConfig] -> Manager -> History -> IO ()
    loop feeds manager history = do
      forM_ feeds $ \feed ->
        pollFeed manager history feed `catchAny` \e ->
          putStrLn ("Feed " <> feedUrl feed <> ": " <> errorMessage e)
      threadDelay (10 * 60 * 1000000)
      loop feeds manager history

    errorMessage :: SomeException -> String
    errorMessage exception = case fromException exception :: Maybe HttpException of
      Just (InvalidUrlException _ _) -> "Invalid feed URL."
      Just (HttpExceptionRequest _ _) -> "Feed HTTP request failed."
      Nothing -> displayException exception
