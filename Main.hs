{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, newChan, readChan, threadDelay)
import Control.Exception.Safe (IOException, SomeException, catchAny, displayException, fromException)
import Control.Monad (foldM, forM, forever, unless, when)
import Data.Aeson (object, (.=))
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
import qualified Discord.Internal.Rest.Prelude as Rest
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Network.HTTP.Req as Req
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.Exit (die)
import System.FilePath ((</>), (<.>))
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (feedItems, getItemDescription, getItemId, getItemLink, getItemTitle)
import Text.Feed.Types (Feed (XMLFeed))

type Seen = Set.Set Text

data FeedConfig = FeedConfig
  { feedName :: String
  , feedUrl :: String
  , feedWebhookId :: Rest.WebhookId
  , feedWebhookToken :: Rest.WebhookToken
  , feedPrefix :: Text
  }

-- The library's webhook options don't expose allowed_mentions or wait=true.
data WebhookRequest a where
  SendWebhook :: Rest.WebhookId -> Rest.WebhookToken -> Text -> WebhookRequest Rest.Message

instance Rest.Request (WebhookRequest a) where
  majorRoute (SendWebhook ident _ _) = "executehk " <> show ident
  jsonRequest (SendWebhook ident token content) = Rest.Post
    (Rest.baseUrl Req./: "webhooks" Req./~ ident Req./~ token)
    (pure (Req.ReqBodyJson (object
      [ "content" .= content
      , "allowed_mentions" .= object ["parse" .= (["everyone", "users", "roles"] :: [Text])]
      ])))
    ("wait" Req.=: True)

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
      request <- parseRequest url `catchAny` \_ -> fail ("Invalid URL for feed " <> name <> ".")
      when (BS.null (host request)) $ fail ("Invalid URL for feed " <> name <> ".")
      (ident, token) <- readWebhook webhook `catchAny` \_ -> fail ("Invalid webhook URL for feed " <> name <> ".")
      when (Text.length prefix >= 999) $ fail ("Prefix too long for feed " <> name <> ".")
      pure (FeedConfig name url ident token prefix)

readWebhook :: String -> IO (Rest.WebhookId, Rest.WebhookToken)
readWebhook url = do
  webhook <- parseRequest url
  credentials <- case reverse (BS.split '/' (path webhook)) of
    token : ident : "webhooks" : _ | not (BS.null token) -> do
      ident' <- maybe (fail "Invalid webhook ID.") pure (readMaybe (BS.unpack ident))
      pure (ident', Rest.DiscordToken (Text.pack (BS.unpack token)))
    _ -> fail "Invalid webhook URL."
  unless (BS.null (queryString webhook)) $ fail "Webhook URL query parameters aren't supported."
  pure credentials

main :: IO ()
main = run `catchAny` \e -> die ("RSS polling failed. " <> errorMessage e)
  where
    run = do
      feeds <- readFeeds "feeds.csv"
      logs <- newChan
      _ <- forkIO $ forever (readChan logs)
      (rest, _) <- Rest.startRestThread (Rest.Auth "") logs
      createDirectoryIfMissing True seenDir
      seen <- forM feeds $ \feed -> loadSeen (seenFile (feedName feed))
      manager <- newTlsManager
      loop feeds manager rest seen

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
      -- A timed-out call stays queued, so let the failure terminate the process.
      result <- maybe (fail "Discord delivery timed out after 60 seconds.") pure =<<
        timeout (60 * 1000000) (Rest.writeRestCall rest $ SendWebhook webhookId webhookToken content)
      case result of
        Right _ -> pure ()
        Left (Rest.RestCallInternalErrorCode code _ _) -> fail ("Discord delivery failed. HTTP " <> show code)
        Left (Rest.RestCallInternalNoParse _ _) -> fail "Invalid Discord response."
        Left (Rest.RestCallInternalHttpException _) -> fail "Discord HTTP request failed."

    loop :: [FeedConfig] -> Manager -> Rest.RestChanHandle -> [Maybe Seen] -> IO ()
    loop feeds manager rest states = do
      next <- forM (zip feeds states) $ \(feed, state) ->
        (Just <$> pollFeed manager (post rest (feedWebhookId feed) (feedWebhookToken feed))
          (feedUrl feed) (feedPrefix feed) (seenFile (feedName feed)) state)
          `catchAny` \e -> fail ("Feed " <> feedName feed <> ": " <> errorMessage e)
      threadDelay (10 * 60 * 1000000)
      loop feeds manager rest next

    errorMessage :: SomeException -> String
    errorMessage exception = case fromException exception :: Maybe IOException of
      Just err -> displayException err
      Nothing -> case fromException exception :: Maybe HttpException of
        Just (InvalidUrlException _ _) -> "Invalid feed URL."
        Just (HttpExceptionRequest _ _) -> "Feed HTTP request failed."
        Nothing -> "Unexpected failure."

fetchEntries :: Manager -> String -> Text -> IO [(Text, Text)]
fetchEntries manager url prefixText = do
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
      -- Keep room for the newline and characters Discord counts twice.
      , Text.length prefix + Text.length link < 1000
      ] $ \(item, link) -> do
        let ident = Text.filter (`notElem` ['\r', '\n']) (valueOr link (snd <$> getItemId item))
            title = Text.replace "@" "＠" $
              valueOr (valueOr "New post" (getItemDescription item)) (getItemTitle item)
            titleLimit = 999 - Text.length prefix - Text.length link
        pure (ident, prefix <> Text.take titleLimit title <> "\n" <> link)
  where
    prefix = if Text.null prefixText then "" else prefixText <> " "
    valueOr fallback value = let text = Text.strip (fromMaybe "" value)
                             in if Text.null text then fallback else text

pollFeed :: Manager -> (Text -> IO ()) -> String -> Text -> FilePath -> Maybe Seen -> IO Seen
pollFeed manager post url prefix seenFile seen = do
  entries <- fetchEntries manager url prefix
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
