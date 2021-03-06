{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- based on yesod-newsfeed

module PodcastFeed
    ( podcastFeed
    , RepPodcast (..)
    , module FeedTypes
    ) where

import Import
import FeedTypes
import Yesod.Core (Route)
import qualified Data.ByteString.Char8 as S8
import Data.Text (pack)
import Data.Text.Lazy (toStrict)
import Text.XML
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Map as Map

newtype RepPodcast = RepPodcast Content
instance HasReps RepPodcast where
    chooseRep (RepPodcast c) _ = return (typeRss, c)

-- | Generate the feed
podcastFeed :: Feed (Route master) -> GHandler sub master RepPodcast
podcastFeed feed = do
    render <- getUrlRender
    return $ RepPodcast $ toContent $ renderLBS def $ template feed render

template :: Feed url -> (url -> Text) -> Document
template Feed {..} render =
    Document (Prologue [] Nothing []) root []
  where
    root = Element "rss" (Map.fromList
             [ ("xmlns:itunes", "http://www.itunes.com/dtds/podcast-1.0.dtd")
             , ("version", "2.0")
             ]) $ return $ NodeElement $ Element "channel" Map.empty $ map NodeElement
        $ Element "{http://www.w3.org/2005/Atom}link" (Map.fromList
            [ ("href", render feedLinkSelf)
            , ("rel", "self")
            , ("type", pack $ S8.unpack typeRss)
            ]) []
        : Element "title" Map.empty [NodeContent feedTitle]
        : Element "link" Map.empty [NodeContent $ render feedLinkHome]
        : Element "description" Map.empty [NodeContent $ toStrict $ renderHtml feedDescription]
        : Element "lastBuildDate" Map.empty [NodeContent $ formatRFC822 feedUpdated]
        : Element "language" Map.empty [NodeContent feedLanguage]
        : Element "itunes:image" (Map.fromList [("href",render feedImage)]) []
        : map entryTemplate feedEntries

entryTemplate :: FeedEntry -> Element
entryTemplate FeedEntry {..} =
    Element "item" Map.empty $ map NodeElement
      [Element "title" Map.empty [NodeContent feedEntryTitle]
      , Element "guid" Map.empty [NodeContent feedEntryLink]
      , Element "pubDate" Map.empty [NodeContent $ formatRFC822 feedEntryUpdated]
      , Element "description" Map.empty [NodeContent $ toStrict $ renderHtml feedEntryContent]
      , case feedEntryEnclosure of
           Just e  -> enclosureTemplate e feedEntryLink
           Nothing -> Element "link" Map.empty [NodeContent feedEntryLink]
      ]

enclosureTemplate :: FeedEnclosure -> Text -> Element
enclosureTemplate FeedEnclosure {..} url =
    Element "enclosure" (Map.fromList
        [ ("url", url)
        , ("length", (pack . show) feedEnclosureLength)
        , ("type", feedEnclosureType)
        ]) []
