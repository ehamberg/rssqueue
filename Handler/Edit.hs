{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handler.Edit where

import Import hiding (length)
import Yesod.Form.Jquery (urlJqueryJs)
import Data.Time.Clock (getCurrentTime)
import Network.Wai (remoteHost)
import Text.Julius (ToJavascript, toJavascript)
import Data.Char (isAlphaNum)
import Data.Maybe (isJust)
import Data.Text (isPrefixOf, append, length, head, find, pack)
import Network.HTTP hiding (getRequest)
import Control.Concurrent (forkIO)
import Text.Blaze (ToMarkup, toMarkup)
import Web.Cookie

instance ToJavascript Identifier where
    toJavascript (Identifier t) = toJavascript t

instance ToMarkup Identifier where
    toMarkup (Identifier t) = toMarkup t

urlBootstrapJs :: a -> Either (Route a) Text
urlBootstrapJs _ = Right "//netdna.bootstrapcdn.com/twitter-bootstrap/2.1.0/js/bootstrap.min.js"

addItemForm :: Html -> MForm RSSQueueApp RSSQueueApp (FormResult (Text,Text), Widget)
addItemForm = renderBootstrap $ (,)
    <$> areq textField (FieldSettings "Title" Nothing (Just "item_title") Nothing []) Nothing
    <*> areq urlField' (FieldSettings "URL"   Nothing (Just "item_url")   Nothing []) Nothing
    where
        urlField' = check validateURL textField
        looksLikeUrl u =
            length u >= 4 && isAlphaNum (head u) && isJust (find (=='.') u)
        validateURL u
          | looksLikeUrl u = Right u
          | otherwise      = Left ("That doesn't look like an URL." :: Text)

getEditR :: Identifier -> Handler RepHtml
getEditR identifier = do
    Entity key queue <- runDB $ getBy404 $ UniqueIdentifier identifier

    items <- runDB $ selectList [QueueItemQueueId ==. key] [Desc QueueItemCreated]
    (widget, enctype) <- generateFormPost addItemForm

    newCookie <- lookupCookie "new"
    let isNew = case newCookie of
                     Just "1" -> True
                     _        -> False

    -- set cookie to 0
    setCookie $ parseSetCookie "new = 0;"

    defaultLayout $ do
        lift getYesod >>= (addScriptEither . urlJqueryJs)
        lift getYesod >>= (addScriptEither . urlBootstrapJs)
        setTitle $ toHtml $ queueTitle queue
        let feedid = toHtml identifier
        $(widgetFile "edit")

postEditR :: Identifier -> Handler RepJson
postEditR identifier@(Identifier i) = do
    Just (Entity key _) <- runDB $ getBy $ UniqueIdentifier identifier

    ((result, _), _) <- runFormPost addItemForm

    case result of
         FormSuccess (title,url) -> do
             let url' = if "http://" `isPrefixOf` url || "https://" `isPrefixOf` url
                           then url
                           else "http://" `append` url

             time <- liftIO getCurrentTime
             ip <- fmap (getIpAddr . remoteHost . reqWaiRequest) getRequest
             item <- runDB $ insert $ QueueItem key title url' time ip Nothing Nothing

             -- fork off a thread that sends a HEAD request to get the added
             -- item's length and MIME type
             runInnerHandler <- handlerToIO
             _ <- liftIO $ forkIO $ runInnerHandler $ updateFileInfo url' item

             -- pass id and identifier to javascript handler so a “delete” link
             -- can be generated
             jsonToRepJson $ (i,unKey item)
         FormFailure errors -> do
             liftIO $ print errors
             jsonToRepJson $ String $ mconcat errors
         _ -> jsonToRepJson $ String "error"
    where updateFileInfo url item = do
              headers <- liftIO $ getResponseHeaders url
              let len = case headers of
                              Nothing -> Nothing
                              Just hs -> lookupHeader HdrContentLength hs
              let typ = case headers of
                              Nothing -> Nothing
                              Just hs -> lookupHeader HdrContentType hs
              let len' = fmap read len
              let typ' = fmap pack typ

              runDB $ update item [QueueItemLength =. len'
                                  ,QueueItemType   =. typ']


deleteDeleteItemR :: Identifier -> QueueItemId -> Handler RepJson
deleteDeleteItemR feedId itemId = do
    Entity key _ <- runDB $ getBy404 $ UniqueIdentifier feedId
    runDB $ deleteWhere [QueueItemQueueId ==. key, QueueItemId ==. itemId]
    jsonToRepJson $ String "deleted"
