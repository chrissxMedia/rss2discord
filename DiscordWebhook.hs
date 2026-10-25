{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiscordWebhook (sendWebhook) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (catch)
import Control.Monad (unless)
import Data.Aeson (decode, encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Version (showVersion)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Paths_rss2discord (version)
import System.Timeout (timeout)
import Text.Read (readMaybe)

sendWebhook :: Manager -> String -> Text -> IO ()
sendWebhook manager url content = do
  request <- parseRequest url `catch` (\(_ :: HttpException) -> fail "Invalid Discord webhook URL.")
  unless (secure request && host request `elem` ["discord.com", "discordapp.com"] &&
          any (`BS.isPrefixOf` path request) ["/api/webhooks/", "/api/v10/webhooks/"] &&
          BS.null (queryString request)) $
    fail "Invalid Discord webhook URL."
  let outgoing = request
        { method = "POST"
        , queryString = "?wait=true"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("User-Agent", "DiscordBot (https://github.com/chrissxMedia/rss2discord, " <> BS.pack (showVersion version) <> ")")
            ]
        , requestBody = RequestBodyLBS (encode (object
            [ "content" .= content
            , "allowed_mentions" .= object ["parse" .= (["everyone", "users", "roles"] :: [Text])]
            ]))
        , redirectCount = 0
        , responseTimeout = responseTimeoutMicro deadline
        }
  result <- timeout deadline (post outgoing `catch` (\(_ :: HttpException) -> fail "Discord HTTP request failed."))
  maybe (fail "Discord delivery timed out after 60 seconds.") pure result
  where
    deadline = 60 * 1000000

    post request = do
      response <- httpLbs request manager
      case statusCode (responseStatus response) of
        200 -> do
          let ident = decode (responseBody response) >>= parseMaybe (withObject "Discord message" (.: "id"))
          unless (maybe False (not . Text.null) ident) $ fail "Invalid Discord response."
        429 -> case retryAfter response of
          Just seconds | seconds >= 0 -> do
            threadDelay (ceiling (min 60 seconds * 1000000))
            post request
          _ -> fail "Invalid Discord rate-limit response."
        code | code `elem` [500, 502] -> threadDelay 1000000 >> post request
             | otherwise -> fail ("Discord delivery failed. HTTP " <> show code)

    retryAfter :: Response BL.ByteString -> Maybe Double
    retryAfter response =
      (decode (responseBody response) >>= parseMaybe (withObject "Discord rate limit" (.: "retry_after")))
        <|> header "Retry-After"
        <|> header "X-RateLimit-Reset-After"
      where
        header name = lookup name (responseHeaders response) >>= readMaybe . BS.unpack
