{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (IOException, SomeException, catchAny, displayException, fromException)
import Control.Monad (foldM, forM, forM_, unless, when)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Csv as Csv
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import DiscordWebhook (sendWebhook)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.Exit (die)
import System.FilePath ((</>), (<.>))
import System.Timeout (timeout)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (feedItems, getItemDescription, getItemId, getItemLink, getItemTitle)
import Text.Feed.Types (Feed (XMLFeed))

type Seen = Set.Set Text

data FeedConfig = FeedConfig
  { feedName :: String
  , feedUrl :: String
  , feedWebhook :: String
  , feedPrefix :: Text
  }

readFeeds :: FilePath -> IO [FeedConfig]
readFeeds file = do
  contents <- BL.readFile file
  -- Cassava accepts an unterminated quoted field at EOF.
  unless (even (BL.count '"' contents)) $ fail "Invalid feeds CSV: unterminated quoted field."
  rows <- either (const (fail "Invalid feeds CSV.")) (pure . toList) (Csv.decode Csv.NoHeader contents)
  feeds <- forM rows $ \fields -> case fields of
    [name, url, webhook] -> readFeed name url webhook ""
    [name, url, webhook, prefix] -> readFeed name url webhook (Text.pack prefix)
    _ -> fail "Expected name, feed URL, webhook URL and optional prefix in feeds CSV."
  let names = map feedName feeds
  when (null feeds) $ fail "Feeds CSV must contain at least one feed."
  unless (all (\name -> not (null name) && all (`elem` validNameChars) name) names) $
    fail "Feed names must contain only ASCII letters, digits, hyphens or underscores."
  unless (Set.size (Set.fromList names) == length names) $ fail "Feed names must be unique."
  pure feeds
  where
    validNameChars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "-_"

    readFeed name url webhook prefix = do
      forM_ [("feed URL", url), ("webhook URL", webhook)] $ \(field, address) -> do
        let message = "Invalid " <> field <> " for feed " <> name <> "."
        request <- parseRequest address `catchAny` \_ -> fail message
        when (BS.null (host request)) $ fail message
      when (Text.length prefix >= 999) $ fail ("Prefix too long for feed " <> name <> ".")
      pure (FeedConfig name url webhook prefix)

main :: IO ()
main = run `catchAny` \e -> die ("RSS polling failed. " <> errorMessage e)
  where
    run = do
      feeds <- readFeeds "feeds.csv"
      createDirectoryIfMissing True seenDir
      seen <- forM feeds $ \feed -> loadSeen (seenFile (feedName feed))
      manager <- newTlsManager
      loop feeds manager seen

    seenDir = "/data"
    seenFile name = seenDir </> name <.> "txt"

    loadSeen :: FilePath -> IO (Maybe Seen)
    loadSeen file = do
      exists <- doesFileExist file
      contents <- if exists then TextIO.readFile file else pure ""
      let ids = filter (not . Text.null) (Text.lines contents)
      pure (if Text.null contents then Nothing else Just (Set.fromList ids))

    loop :: [FeedConfig] -> Manager -> [Maybe Seen] -> IO ()
    loop feeds manager states = do
      next <- forM (zip feeds states) $ \(feed, state) ->
        (Just <$> pollFeed manager feed (seenFile (feedName feed)) state)
          `catchAny` \e -> fail ("Feed " <> feedName feed <> ": " <> errorMessage e)
      threadDelay (10 * 60 * 1000000)
      loop feeds manager next

    errorMessage :: SomeException -> String
    errorMessage exception = case fromException exception :: Maybe IOException of
      Just err -> displayException err
      Nothing -> case fromException exception :: Maybe HttpException of
        Just (InvalidUrlException _ _) -> "Invalid feed URL."
        Just (HttpExceptionRequest _ _) -> "Feed HTTP request failed."
        Nothing -> "Unexpected failure."

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
      , not (Text.null link)
      -- Keep room for the newline and characters Discord counts twice.
      , Text.length link < 1000
      ] $ \(item, link) -> do
        when (Text.length prefix + Text.length link >= 1000) $
          fail "Prefix and feed link exceed the message length limit."
        let ident = Text.filter (`notElem` ['\r', '\n']) (valueOr link (snd <$> getItemId item))
            title = Text.replace "@" "＠" $
              valueOr (valueOr "New post" (getItemDescription item)) (getItemTitle item)
            titleLimit = 999 - Text.length prefix - Text.length link
        pure (ident, prefix <> Text.take titleLimit title <> "\n" <> link)
  where
    prefix = if Text.null (feedPrefix feed) then "" else feedPrefix feed <> " "
    valueOr fallback value = let text = Text.strip (fromMaybe "" value)
                             in if Text.null text then fallback else text

pollFeed :: Manager -> FeedConfig -> FilePath -> Maybe Seen -> IO Seen
pollFeed manager feed seenFile seen = do
  entries <- fetchEntries manager feed
  case seen of
    Nothing -> do
      let state = Set.fromList (map fst entries)
          temporary = seenFile <> ".tmp"
      -- A newline marks an initialized feed even when it has no entries.
      TextIO.writeFile temporary (Text.unlines (Set.toList state) <> "\n")
      renameFile temporary seenFile
      pure state
    Just state -> foldM step state (reverse entries)
  where
    step state (ident, content)
      | Set.member ident state = pure state
      | otherwise = do
          sendWebhook manager (feedWebhook feed) content
          -- shortcut: interrupted appends can leave a partial last line, use atomic writes before deploying.
          TextIO.appendFile seenFile (ident <> "\n")
          pure (Set.insert ident state)
