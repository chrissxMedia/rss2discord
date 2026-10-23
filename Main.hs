{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, newChan, readChan, threadDelay)
import Control.Exception.Safe (IOException, SomeException, catchAny, displayException, fromException)
import Control.Monad (foldM, forM, forM_, forever, unless, when)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Csv as Csv
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Discord.Internal.Rest as Rest
import qualified Discord.Requests as Discord
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.Environment (getEnv)
import System.Exit (die)
import System.FilePath ((</>), (<.>))
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (feedItems, getItemDescription, getItemId, getItemLink, getItemTitle)
import Text.Feed.Types (Feed (XMLFeed))

type Seen = Set.Set Text

readFeeds :: FilePath -> IO [(String, String)]
readFeeds file = do
  contents <- BL.readFile file
  -- Cassava accepts an unterminated quoted field at EOF.
  unless (even (BL.count '"' contents)) $ fail "Invalid feeds CSV: unterminated quoted field."
  feeds <- either (fail . ("Invalid feeds CSV: " <>)) (pure . toList) (Csv.decode Csv.NoHeader contents)
  let names = map fst feeds
  when (null feeds) $ fail "Feeds CSV must contain at least one feed."
  unless (all (\name -> not (null name) && all (`elem` validNameChars) name) names) $
    fail "Feed names must contain only ASCII letters, digits, hyphens or underscores."
  unless (Set.size (Set.fromList names) == length names) $ fail "Feed names must be unique."
  forM_ feeds $ \(name, url) -> do
    request <- parseRequest url `catchAny` \_ -> fail ("Invalid URL for feed " <> name <> ".")
    when (BS.null (host request)) $ fail ("Invalid URL for feed " <> name <> ".")
  pure feeds
  where
    validNameChars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "-_"

main :: IO ()
main = run `catchAny` \e -> die ("RSS polling failed. " <> errorMessage e)
  where
    run = do
      feeds <- readFeeds "feeds.csv"
      (webhookId, webhookToken) <- readWebhook
      logs <- newChan
      _ <- forkIO $ forever (readChan logs)
      (rest, _) <- Rest.startRestThread (Rest.Auth "") logs
      createDirectoryIfMissing True seenDir
      seen <- forM feeds $ \(name, _) -> loadSeen (seenFile name)
      manager <- newTlsManager
      loop feeds manager (post rest webhookId webhookToken) seen

    readWebhook :: IO (Rest.WebhookId, Rest.WebhookToken)
    readWebhook = do
      webhook <- getEnv "DISCORD_WEBHOOK_URL" >>= \url ->
        parseRequest url `catchAny` \_ -> fail "Invalid DISCORD_WEBHOOK_URL."
      credentials <- case reverse (BS.split '/' (path webhook)) of
        token : ident : "webhooks" : _ | not (BS.null token) -> do
          ident' <- maybe (fail "Invalid webhook ID.") pure (readMaybe (BS.unpack ident))
          pure (ident', Rest.DiscordToken (Text.pack (BS.unpack token)))
        _ -> fail "Invalid webhook URL."
      unless (BS.null (queryString webhook)) $ fail "Webhook URL query parameters aren't supported."
      pure credentials

    seenDir = "/data"
    seenFile name = seenDir </> name <.> "txt"

    loadSeen :: FilePath -> IO (Maybe Seen)
    loadSeen file = do
      exists <- doesFileExist file
      contents <- if exists then TextIO.readFile file else pure ""
      let ids = filter (not . Text.null) (Text.lines contents)
      pure (if Text.null contents then Nothing else Just (Set.fromList ids))

    post :: Rest.RestChanHandle -> Rest.WebhookId -> Rest.WebhookToken -> Text -> IO ()
    post rest webhookId webhookToken content = do
      -- shortcut: the library omits wait=true, use a supporting API for delivery confirmation.
      -- A timed-out call stays queued, so let the failure terminate the process.
      result <- maybe (fail "Discord delivery timed out after 60 seconds.") pure =<<
        timeout (60 * 1000000) (Rest.writeRestCall rest $ Discord.ExecuteWebhook webhookId webhookToken
          (Discord.ExecuteWebhookWithTokenOpts Nothing (Discord.WebhookContentText content)))
      case result of
        Right () -> pure ()
        Left (Rest.RestCallInternalErrorCode code _ _) -> fail ("Discord delivery failed. HTTP " <> show code)
        Left (Rest.RestCallInternalNoParse _ _) -> fail "Invalid Discord response."
        Left (Rest.RestCallInternalHttpException _) -> fail "Discord HTTP request failed."

    loop :: [(String, String)] -> Manager -> (Text -> IO ()) -> [Maybe Seen] -> IO ()
    loop feeds manager send states = do
      next <- forM (zip feeds states) $ \((name, url), state) ->
        (Just <$> pollFeed manager send url (seenFile name) state)
          `catchAny` \e -> fail ("Feed " <> name <> ": " <> errorMessage e)
      threadDelay (10 * 60 * 1000000)
      loop feeds manager send next

    errorMessage :: SomeException -> String
    errorMessage exception = case fromException exception :: Maybe IOException of
      Just err -> displayException err
      Nothing -> case fromException exception :: Maybe HttpException of
        Just (InvalidUrlException _ _) -> "Invalid feed URL."
        Just (HttpExceptionRequest _ _) -> "Feed HTTP request failed."
        Nothing -> "Unexpected failure."

fetchEntries :: Manager -> String -> IO [(Text, Text)]
fetchEntries manager url = do
  request <- parseRequest url
  response <- maybe (fail "Feed request timed out.") pure =<< timeout (60 * 1000000) (httpLbs request manager)
  unless (statusCode (responseStatus response) == 200) $
    fail ("Feed request failed. HTTP " <> show (statusCode (responseStatus response)))
  case parseFeedSource (responseBody response) of
    Nothing -> fail "Invalid feed."
    Just (XMLFeed _) -> fail "Expected RSS or Atom."
    Just feed -> forM
      [ (item, link)
      | item <- feedItems feed
      , let link = Text.strip (fromMaybe "" (getItemLink item))
      , not (Text.null link)
      -- Leave room for a newline and characters that take two UTF-16 units.
      , Text.length link < 1000
      ] $ \(item, link) -> do
        let ident = Text.filter (`notElem` ['\r', '\n']) (valueOr link (snd <$> getItemId item))
            title = Text.replace "@" "＠" $
              valueOr (valueOr "New post" (getItemDescription item)) (getItemTitle item)
        pure (ident, Text.take (999 - Text.length link) title <> "\n" <> link)
  where
    valueOr fallback value = let text = Text.strip (fromMaybe "" value)
                             in if Text.null text then fallback else text

pollFeed :: Manager -> (Text -> IO ()) -> String -> FilePath -> Maybe Seen -> IO Seen
pollFeed manager post url seenFile seen = do
  entries <- fetchEntries manager url
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
          post content
          -- shortcut: interrupted appends can leave a partial last line, use atomic writes before deploying.
          TextIO.appendFile seenFile (ident <> "\n")
          pure (Set.insert ident state)
