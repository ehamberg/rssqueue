{-# LANGUAGE OverloadedStrings #-}

module Handler.View where

import Import
import Yesod.Form.Jquery (urlJqueryJs)
import Data.Text (append)
import qualified Data.List (length)

getViewR :: Identifier -> Handler RepHtml
getViewR identifier = do
    Entity key queue <- runDB $ getBy404 $ UniqueShareId identifier

    items <- runDB $ selectList [QueueItemQueueId ==. key] [Desc QueueItemCreated]

    let showDelete = False
    let feedid = identifier
    let feedinfo = $(widgetFile "feedinfo")
    let itemlist = $(widgetFile "itemlist")

    defaultLayout $ do
        lift getYesod >>= (addScriptEither . urlJqueryJs)
        lift getYesod >>= (addScriptEither . urlBootstrapJs)
        setTitle $ toHtml $ queueTitle queue `append` " – RSSQueue"
        $(widgetFile "view")
