{-# LANGUAGE OverloadedStrings #-}

module Handler.View where

import Import
import Data.Text (append)
import Data.Maybe (fromMaybe)
import qualified Data.List (length)

getViewR :: Identifier -> Handler RepHtml
getViewR identifier = do
    Entity key queue <- runDB $ getBy404 $ UniqueShareId identifier

    items <- runDB $ selectList [QueueItemQueueId ==. key] [Desc QueueItemCreated]

    let isEditView = False
    let feedid = identifier
    let feedinfo = $(widgetFile "feedinfo")
    let itemlist = $(widgetFile "itemlist")

    defaultLayout $ do
        setTitle $ toHtml $ queueTitle queue `append` " – RSSQueue"
        $(widgetFile "view")
