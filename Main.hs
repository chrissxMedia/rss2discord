{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, isEmptyMVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Exception.Safe (SomeException, catchAny, displayException, fromException)
import Control.Monad (forM_, unless, void)
import Feed (pollFeed)
import FeedConfig (FeedConfig (..), readFeeds)
import History (History, withHistory)
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
      withHistory "/data/history.sqlite3" $ \history -> loop stopping feeds manager history

    loop :: MVar () -> [FeedConfig] -> Manager -> History -> IO ()
    loop stopping feeds manager history = do
      let shouldStop = not <$> isEmptyMVar stopping
      forM_ feeds $ \feed -> do
        stopped <- shouldStop
        unless stopped $
          pollFeed shouldStop manager history feed `catchAny` \e ->
            putStrLn ("Feed " <> feedUrl feed <> ": " <> errorMessage e)
      void $ timeout (10 * 60 * 1000000) (readMVar stopping)
      stopped <- shouldStop
      unless stopped $ loop stopping feeds manager history

    errorMessage :: SomeException -> String
    errorMessage exception = case fromException exception :: Maybe HttpException of
      Just (InvalidUrlException _ _) -> "Invalid feed URL."
      Just (HttpExceptionRequest _ _) -> "Feed HTTP request failed."
      Nothing -> displayException exception
