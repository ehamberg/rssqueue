{-# LANGUAGE OverloadedStrings #-}

module Handler.View where

import Import
import Data.Text (append)

getViewR :: Identifier -> Handler RepHtml
getViewR identifier = do
    Entity key queue <- runDB $ getBy404 $ UniqueShareId identifier

    items <- runDB $ selectList [QueueItemQueueId ==. key] [Desc QueueItemCreated]

    let showDelete = False
    let itemlist = $(widgetFile "itemlist")

    defaultLayout $ do
        lift getYesod >>= (addScriptEither . urlBootstrapJs)
        setTitle $ toHtml $ queueTitle queue `append` " – RSSQueue"
        let feedid = toHtml identifier
        $(widgetFile "view")
