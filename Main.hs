{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, isEmptyMVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Exception.Safe (SomeException, catchAny, displayException, fromException, tryAny)
import Control.Monad (forM_, unless, void)
import Data.List (find)
import qualified Data.Text as Text
import ErrorReporting (reportFeedErrors)
import Feed (pollFeed)
import FeedConfig (FeedConfig (..), internalFeedUrl, readFeeds)
import History (History, deleteItemsExcept, withHistory)
import Network.HTTP.Client (HttpException (..), Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import System.Timeout (timeout)

main :: IO ()
main = run `catchAny` \e -> die ("rss2discord failed. " <> errorMessage e)
  where
    run = do
      stopping <- newEmptyMVar
      forM_ [sigTERM, sigINT] $ \signal ->
        void $ installHandler signal (Catch (void $ tryPutMVar stopping ())) Nothing
      feeds <- readFeeds "feeds.csv"
      createDirectoryIfMissing True "/data"
      manager <- newTlsManager
      withHistory "/data/history.sqlite3" $ \history -> do
        let activeUrls = if any ((== internalFeedUrl) . feedUrl) feeds
              then [Text.pack (feedUrl feed) | feed <- feeds, feedUrl feed /= internalFeedUrl]
              else []
        deleteItemsExcept history (Text.pack internalFeedUrl) activeUrls
        loop stopping feeds manager history

    loop :: MVar () -> [FeedConfig] -> Manager -> History -> IO ()
    loop stopping feeds manager history = do
      let shouldStop = not <$> isEmptyMVar stopping
          internal = find ((== internalFeedUrl) . feedUrl) feeds
      forM_ (zip [1 :: Int ..] feeds) $ \(row, feed) -> do
        stopped <- shouldStop
        unless (stopped || feedUrl feed == internalFeedUrl) $ do
          result <- tryAny (pollFeed shouldStop manager history feed)
          let errors = either (:[]) id result
          forM_ errors $ \err ->
            putStrLn ("Feed at feeds.csv row " <> show row <> ": " <> errorMessage err)
          stoppingNow <- shouldStop
          unless stoppingNow $ forM_ internal $ \config ->
            reportFeedErrors manager history config row (feedUrl feed) errors
      void $ timeout (10 * 60 * 1000000) (readMVar stopping)
      stopped <- shouldStop
      unless stopped $ loop stopping feeds manager history

    errorMessage :: SomeException -> String
    errorMessage exception = case fromException exception :: Maybe HttpException of
      Just (InvalidUrlException _ _) -> "Invalid feed URL."
      Just (HttpExceptionRequest _ _) -> "Feed HTTP request failed."
      Nothing -> displayException exception
