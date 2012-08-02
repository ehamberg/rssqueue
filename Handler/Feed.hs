{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Feed where

import Import
import Data.Time.Clock (getCurrentTime)
import Network.Wai (remoteHost)
import Network.Socket (SockAddr (..))
import Data.Text (pack)
import Data.Maybe (fromMaybe)

getFeedR :: FeedId -> Handler RepHtml
getFeedR feedId = do
    feed <- runDB $ get404 feedId
    items <- runDB $ selectList [FeedItemFeedId ==. feedId] [Desc FeedItemCreated]
                    >>= mapM (\(Entity _ v) -> return v)
    defaultLayout $ do
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"
        addButtonId <- lift newIdent
        titleFieldId <- lift newIdent
        urlFieldId <- lift newIdent
        setTitle $ toHtml $ feedTitle feed
        $(widgetFile "feed")

getIpAddr :: SockAddr -> Text
getIpAddr (SockAddrInet   _ addr)     = (pack . show) addr
getIpAddr (SockAddrInet6 _ _ addr _ ) = (pack . show) addr
getIpAddr _                           = pack ""

postFeedR :: FeedId -> Handler RepJson
postFeedR feedId = do
    title <- fmap (fromMaybe "") $ lookupPostParam "new_title"
    url   <- fmap (fromMaybe "") $ lookupPostParam "new_url"
    time <- liftIO getCurrentTime
    ip <- fmap (getIpAddr . remoteHost . reqWaiRequest) getRequest

    _ <- runDB $ insert $ FeedItem feedId title url time ip
    jsonToRepJson $ String "success"
