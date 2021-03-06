{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Feed where

import Import
import PodcastFeed
import Data.Time.Clock (getCurrentTime)
import Data.Maybe (fromMaybe)
import Data.List (head)

getFeedR :: Identifier -> Handler RepPodcast
getFeedR identifier = do
    Entity key queue <- runDB $ getBy404 $ UniqueShareId identifier

    items <- runDB $ selectList [QueueItemQueueId ==. key] [Desc QueueItemCreated] >>= mapM (\(Entity _ v) -> return v)
    is <- mapM toFeedEntry items

    let lastItemTime = if null is
                          then Nothing
                          else Just $ feedEntryUpdated . head $ is

    time <- liftIO getCurrentTime

    podcastFeed Feed { feedTitle = queueTitle queue
                     , feedAuthor = "RSSQueue.com"
                     , feedLinkSelf = FeedR identifier
                     , feedLinkHome = HomeR
                     , feedImage = StaticR img_feedlogo_png
                     , feedDescription = "Feed from RSSQueue.com"
                     , feedLanguage = "en"
                     , feedUpdated = fromMaybe time lastItemTime
                     , feedEntries = is
                     }

toFeedEntry :: QueueItem -> Handler FeedEntry
toFeedEntry item = return FeedEntry
                   { feedEntryLink      = queueItemUri item
                   , feedEntryUpdated   = queueItemCreated item
                   , feedEntryTitle     = queueItemTitle item
                   , feedEntryContent   = ""
                   , feedEntryEnclosure = Just enclosure
                   }
              where enclosure = FeedEnclosure
                      { feedEnclosureUrl    = queueItemUri item
                      , feedEnclosureLength = fromMaybe 0 $ queueItemLength item
                      , feedEnclosureType   = fromMaybe "" $ queueItemType item
                      }
