{-# LANGUAGE OverloadedStrings #-}

module ErrorReporting (reportFeedErrors, safeErrorMessage) where

import Control.Exception.Safe (IOException, SomeException, catchAny, fromException)
import Control.Monad (when)
import Data.List (nub)
import qualified Data.Text as Text
import DiscordWebhook (editWebhook, sendWebhook)
import FeedConfig (FeedConfig (..), internalFeedUrl)
import History (History, ItemState (..), deleteItem, lookupItem, saveItem)
import Network.HTTP.Client (HttpException (..), Manager)
import System.IO.Error (ioeGetErrorString, isUserError)

reportFeedErrors :: Manager -> History -> FeedConfig -> Int -> String -> [SomeException] -> IO ()
reportFeedErrors manager history internal row sourceUrl errors =
  report `catchAny` \err -> putStrLn ("Internal error reporting failed. " <> safeErrorMessage err)
  where
    url = Text.pack internalFeedUrl
    ident = Text.pack sourceUrl
    webhook = feedWebhook internal
    label = "Feed at feeds.csv row " <> Text.pack (show row)
    prefix = if Text.null (feedPrefix internal) then "" else feedPrefix internal <> " "
    format message = prefix <> Text.take (999 - Text.length prefix) message

    report = do
      previous <- lookupItem history url ident
      case errors of
        [] -> case previous of
          Just item -> do
            -- Close the episode even if the resolution message cannot be delivered.
            deleteItem history url ident
            case itemStateMessageId item of
              Just messageId -> editWebhook manager webhook messageId (format (label <> " recovered."))
              Nothing -> pure ()
          Nothing -> pure ()
        _ -> do
          let content = format (label <> " failed: " <> Text.intercalate "; " (nub (map (Text.pack . safeErrorMessage) errors)))
          case previous >>= itemStateMessageId of
            Just messageId -> when (fmap itemStateContent previous /= Just content) $ do
              editWebhook manager webhook messageId content
              saveItem history (ItemState url ident (Just messageId) content)
            Nothing -> do
              messageId <- sendWebhook manager webhook content
              saveItem history (ItemState url ident (Just messageId) content)

safeErrorMessage :: SomeException -> String
safeErrorMessage exception = case fromException exception :: Maybe HttpException of
  Just (InvalidUrlException _ _) -> "Invalid feed URL."
  Just (HttpExceptionRequest _ _) -> "Feed HTTP request failed."
  Nothing -> case fromException exception :: Maybe IOException of
    Just err | isUserError err, safeMessage (ioeGetErrorString err) -> ioeGetErrorString err
             | otherwise -> "File I/O failed."
    Nothing -> "Unexpected failure."
  where
    safeMessage message = message `elem`
      [ "Feed request timed out."
      , "Invalid feed."
      , "Expected RSS or Atom."
      , "Prefix and feed link exceed the message length limit."
      , "Invalid Discord webhook URL."
      , "Invalid Discord message ID."
      , "Discord HTTP request failed."
      , "Discord delivery timed out after 60 seconds."
      , "Invalid Discord response."
      , "Invalid Discord rate-limit response."
      ] || any (\prefix -> any (\code -> message == prefix <> show code) [100 :: Int .. 599])
        ["Feed request failed. HTTP ", "Discord delivery failed. HTTP "]
