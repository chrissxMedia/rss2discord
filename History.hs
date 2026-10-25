{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module History (History, ItemState (..), withHistory, initializeFeed, lookupItem, saveItem) where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite (SqlBackend, runMigration, runSqlConn, withSqliteConn)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ItemState
    feedUrl Text
    rssId Text
    messageId Text Maybe
    content Text
    UniqueFeedItem feedUrl rssId
|]

type History = SqlBackend

withHistory :: Text -> (History -> IO a) -> IO a
withHistory file action = runNoLoggingT $ withSqliteConn file $ \backend -> liftIO $ do
  runSqlConn (runMigration migrateAll) backend
  action backend

initializeFeed :: History -> Text -> [(Text, Text)] -> IO Bool
initializeFeed history url entries = flip runSqlConn history $ do
  marker <- getBy (UniqueFeedItem url "")
  case marker of
    Just _ -> pure False
    Nothing -> do
      -- The empty ID records initialization even when the feed has no items.
      insert_ (ItemState url "" Nothing "")
      forM_ entries $ \(ident, content) ->
        void (insertUnique (ItemState url ident Nothing content))
      pure True

lookupItem :: History -> Text -> Text -> IO (Maybe ItemState)
lookupItem history url ident =
  fmap entityVal <$> runSqlConn (getBy (UniqueFeedItem url ident)) history

saveItem :: History -> ItemState -> IO ()
saveItem history item = void $ runSqlConn
  (upsert item
    [ ItemStateMessageId =. itemStateMessageId item
    , ItemStateContent =. itemStateContent item
    ]) history
