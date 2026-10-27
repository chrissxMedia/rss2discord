{-# LANGUAGE OverloadedStrings #-}

module Feed (fetchEntries, pollFeed) where

import Control.Exception.Safe (displayException, tryAny)
import Control.Monad (forM, forM_, unless, when)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isControl, isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import DiscordWebhook (editWebhook, sendWebhook)
import FeedConfig (FeedConfig (..))
import History (History, ItemState (..), initializeFeed, lookupItem, saveItem)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.Timeout (timeout)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (feedItems, getItemDescription, getItemId, getItemLink, getItemTitle)
import Text.Feed.Types (Feed (XMLFeed))

fetchEntries :: Manager -> FeedConfig -> IO [(Text, Text)]
fetchEntries manager feed = do
  request <- parseRequest (feedUrl feed)
  response <- maybe (fail "Feed request timed out.") pure =<< timeout (60 * 1000000) (httpLbs request manager)
  unless (statusCode (responseStatus response) == 200) $
    fail ("Feed request failed. HTTP " <> show (statusCode (responseStatus response)))
  case parseFeedSource (responseBody response) of
    Nothing -> fail "Invalid feed."
    Just (XMLFeed _) -> fail "Expected RSS or Atom."
    Just parsed -> forM
      [ (item, link)
      | item <- feedItems parsed
      , let link = Text.strip (fromMaybe "" (getItemLink item))
      , Just target <- [parseRequest (Text.unpack link) :: Maybe Request]
      , not (BS.null (host target))
      , not (Text.any (\c -> isSpace c || isControl c || c `elem` ['<', '>']) link)
      -- Keep room for the newline and characters Discord counts twice.
      , Text.length link < 1000
      ] $ \(item, link) -> do
        when (Text.length prefix + Text.length link >= 1000) $
          fail "Prefix and feed link exceed the message length limit."
        let ident = valueOr link (snd <$> getItemId item)
            title = Text.replace "@" "＠" $
              valueOr (valueOr "New post" (getItemDescription item)) (getItemTitle item)
            titleLimit = 999 - Text.length prefix - Text.length link
        pure (ident, prefix <> Text.take titleLimit title <> "\n" <> link)
  where
    prefix = if Text.null (feedPrefix feed) then "" else feedPrefix feed <> " "
    valueOr fallback value = let text = Text.strip (fromMaybe "" value)
                             in if Text.null text then fallback else text

pollFeed :: Manager -> History -> FeedConfig -> IO ()
pollFeed manager history feed = do
  entries <- fetchEntries manager feed
  initialized <- initializeFeed history url entries
  unless initialized $ forM_ (reverse entries) $ \(ident, content) -> do
    previous <- lookupItem history url ident
    case previous of
      Nothing -> do
        messageId <- sendWebhook manager webhook content
        -- shortcut: a crash between sending and saving can duplicate a message, revisit if Discord adds idempotent sends.
        saveItem history (ItemState url ident (Just messageId) content)
      Just item -> case itemStateMessageId item of
        Just messageId | itemStateContent item /= content -> do
          result <- tryAny (editWebhook manager webhook messageId content)
          case result of
            Left err -> putStrLn ("Feed " <> feedUrl feed <> ", item " <> Text.unpack ident <> ": " <> displayException err)
            Right () -> saveItem history (item { itemStateContent = content })
        _ -> pure ()
  where
    url = Text.pack (feedUrl feed)
    webhook = feedWebhook feed
