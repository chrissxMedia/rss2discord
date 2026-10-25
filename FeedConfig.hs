{-# LANGUAGE OverloadedStrings #-}

module FeedConfig (FeedConfig (..), readFeeds) where

import Control.Exception.Safe (catchAny)
import Control.Monad (foldM_, forM, forM_, unless, when)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Csv as Csv
import Data.Foldable (toList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client (Request (host), parseRequest)

data FeedConfig = FeedConfig
  { feedUrl :: String
  , feedWebhook :: String
  , feedPrefix :: Text
  }

readFeeds :: FilePath -> IO [FeedConfig]
readFeeds file = do
  contents <- BL.readFile file
  -- Cassava accepts an unterminated quoted field at EOF.
  unless (even (BL.count '"' contents)) $ fail "Invalid feeds CSV: unterminated quoted field."
  rows <- either (const (fail "Invalid feeds CSV.")) (pure . toList) (Csv.decode Csv.NoHeader contents)
  feeds <- forM (zip [1 :: Int ..] rows) $ \(row, fields) -> case fields of
    [url, webhook] -> readFeed row url webhook ""
    [url, webhook, prefix] -> readFeed row url webhook (Text.pack prefix)
    _ -> fail (rowMessage row "Expected feed URL, webhook URL and optional prefix.")
  let urls = map feedUrl feeds
  when (null feeds) $ fail "Feeds CSV must contain at least one feed."
  foldM_ (\seen (row, url) -> do
    when (Set.member url seen) $ fail (rowMessage row "Duplicate feed URL.")
    pure (Set.insert url seen)) Set.empty (zip [1 :: Int ..] urls)
  pure feeds
  where
    rowMessage row message = "Feeds CSV row " <> show row <> ": " <> message

    readFeed row url webhook prefix = do
      forM_ [("feed URL", url), ("webhook URL", webhook)] $ \(field, address) -> do
        let message = rowMessage row ("Invalid " <> field <> ".")
        request <- parseRequest address `catchAny` \_ -> fail message
        when (BS.null (host request)) $ fail message
      when (Text.length prefix >= 999) $ fail (rowMessage row "Prefix too long.")
      pure (FeedConfig url webhook prefix)
