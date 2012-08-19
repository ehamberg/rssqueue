module FeedTypes
    ( Feed (..)
    , FeedEntry (..)
    , FeedEnclosure (..)
    ) where

import Import
import Data.Time.Clock  (UTCTime)
import Data.Int         (Int64)

-- | The overall feed
data Feed url = Feed
    { feedTitle       :: Text
    , feedLinkSelf    :: url
    , feedLinkHome    :: url
    , feedAuthor      :: Text
    , feedDescription :: Html
    , feedLanguage    :: Text
    , feedImage       :: url
    , feedUpdated     :: UTCTime
    , feedEntries     :: [FeedEntry]
    }

-- | Each feed entry
data FeedEntry = FeedEntry
    { feedEntryLink      :: Text
    , feedEntryUpdated   :: UTCTime
    , feedEntryTitle     :: Text
    , feedEntryContent   :: Html
    , feedEntryEnclosure :: Maybe FeedEnclosure
    }

data FeedEnclosure = FeedEnclosure
    { feedEnclosureUrl    :: Text
    , feedEnclosureLength :: Int64
    , feedEnclosureType   :: Text
    }
