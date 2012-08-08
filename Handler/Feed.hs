{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Feed where

import Import
import Yesod.Feed
import Data.Time.Clock (getCurrentTime)
import Data.Maybe (fromMaybe)
import Data.List (head)

getFeedR :: Identifier -> Handler RepAtomRss
getFeedR identifier = do
    Entity key queue <- runDB $ getBy404 $ UniqueIdentifier identifier

    items <- runDB $ selectList [QueueItemQueueId ==. key] [Desc QueueItemCreated] >>= mapM (\(Entity _ v) -> return v)
    is <- mapM toFeedEntry items

    let lastItemTime = if null is
                          then Nothing
                          else Just $ feedEntryUpdated . head $ is

    time <- liftIO getCurrentTime

    newsFeed Feed { feedTitle = queueTitle queue
                  , feedAuthor = "RSSQueue.com"
                  , feedLinkSelf = FeedR identifier
                  , feedLinkHome = HomeR
                  , feedDescription = "Feed from RSSQueue.com"
                  , feedLanguage = "en"
                  , feedUpdated = fromMaybe time lastItemTime
                  , feedEntries = is
                  }

toFeedEntry :: QueueItem -> Handler (FeedEntry (Route RSSQueueApp))
toFeedEntry item = return FeedEntry
                   { feedEntryLink    = GetR $ queueItemUri item
                   , feedEntryUpdated = queueItemCreated item
                   , feedEntryTitle   = queueItemTitle item
                   , feedEntryContent = ""
                   }
