{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Feed where

import Import
import Data.Time.Clock (getCurrentTime)
import Network.Wai (remoteHost)
import Data.Maybe (fromMaybe)

getFeedR :: Text -> Handler RepHtml
getFeedR identifier = do
    Entity key feed <- runDB $ getBy404 $ UniqueIdentifier identifier
    liftIO $ print (Entity key feed)

    items <- runDB $ selectList [FeedItemFeedId ==. key] [Desc FeedItemCreated] >>= mapM (\(Entity _ v) -> return v)
    defaultLayout $ do
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"
        addButtonId <- lift newIdent
        titleFieldId <- lift newIdent
        urlFieldId <- lift newIdent
        setTitle $ toHtml $ feedTitle feed
        $(widgetFile "feed")

postFeedR :: Text -> Handler RepJson
postFeedR identifier = do
    Just (Entity key _) <- runDB $ getBy $ UniqueIdentifier identifier

    title <- fmap (fromMaybe "") $ lookupPostParam "new_title"
    url   <- fmap (fromMaybe "") $ lookupPostParam "new_url"
    time <- liftIO getCurrentTime
    ip <- fmap (getIpAddr . remoteHost . reqWaiRequest) getRequest

    _ <- runDB $ insert $ FeedItem key title url time ip
    jsonToRepJson $ String "success"
