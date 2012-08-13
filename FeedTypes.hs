module FeedTypes
    ( Feed (..)
    , FeedEntry (..)
    , FeedEnclosure (..)
    ) where

import Import
import Text.Hamlet      (Html)
import Data.Time.Clock  (UTCTime)
import Data.Text        (Text)
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
    , feedEntries     :: [FeedEntry url]
    }

-- | Each feed entry
data FeedEntry url = FeedEntry
    { feedEntryLink      :: url
    , feedEntryUpdated   :: UTCTime
    , feedEntryTitle     :: Text
    , feedEntryContent   :: Html
    , feedEntryEnclosure :: Maybe (FeedEnclosure url)
    }

data FeedEnclosure url = FeedEnclosure
    { feedEnclosureUrl    :: url
    , feedEnclosureLength :: Int64
    , feedEnclosureType   :: Text
    }
