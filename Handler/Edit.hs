{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handler.Edit where

import Import
import Data.Time.Clock (getCurrentTime)
import Network.Wai (remoteHost)
import Data.Maybe (fromMaybe)
import Text.Julius (ToJavascript, toJavascript)

instance ToJavascript Identifier where
    toJavascript (Identifier t) = toJavascript t

getEditR :: Identifier -> Handler RepHtml
getEditR identifier = do
    Entity key queue <- runDB $ getBy404 $ UniqueIdentifier identifier

    items <- runDB $ selectList [QueueItemQueueId ==. key] [Desc QueueItemCreated] >>= mapM (\(Entity _ v) -> return v)
    defaultLayout $ do
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"
        addButtonId <- lift newIdent
        titleFieldId <- lift newIdent
        urlFieldId <- lift newIdent
        setTitle $ toHtml $ queueTitle queue
        $(widgetFile "edit")

postEditR :: Identifier -> Handler RepJson
postEditR identifier = do
    Just (Entity key _) <- runDB $ getBy $ UniqueIdentifier identifier

    title <- fmap (fromMaybe "") $ lookupPostParam "new_title"
    url   <- fmap (fromMaybe "") $ lookupPostParam "new_url"
    time <- liftIO getCurrentTime
    ip <- fmap (getIpAddr . remoteHost . reqWaiRequest) getRequest

    _ <- runDB $ insert $ QueueItem key title url time ip
    jsonToRepJson $ String "success"
