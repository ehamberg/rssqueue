{-# LANGUAGE OverloadedStrings #-}

module Handler.Edit where

import Import hiding (length)
import Data.Time.Clock (getCurrentTime)
import Network.Wai (remoteHost)
import Data.Char (isAlphaNum)
import Data.Maybe (isJust, fromMaybe)
import Data.Text (isPrefixOf, append, length, head, find, pack)
import Network.HTTP hiding (getRequest)
import Control.Concurrent (forkIO)
import Control.Monad (when)
import qualified Data.List (length)

renderBootstrap' :: FormRender sub master a
renderBootstrap' aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
    let widget = $(widgetFile "newitemform")
    return (res, widget)

addItemForm :: Html -> MForm RSSQueueApp RSSQueueApp (FormResult (Text,Text), Widget)
addItemForm = renderBootstrap' $ (,)
    <$> areq textField (FieldSettings "Title" Nothing (Just "item_title") Nothing []) Nothing
    <*> areq urlField' (FieldSettings "URL"   Nothing (Just "item_url")   Nothing []) Nothing
    where
        urlField' = check validateURL textField
        looksLikeUrl u = length u >= 4 && isAlphaNum (head u) && isJust (find (=='.') u)
        validateURL u
          | looksLikeUrl u = Right u
          | otherwise      = Left ("That doesn't look like an URL." :: Text)

getEditR :: Identifier -> Handler RepHtml
getEditR identifier = do
    Entity key queue <- runDB $ getBy404 $ UniqueIdentifier identifier

    items <- runDB $ selectList [QueueItemQueueId ==. key] [Desc QueueItemCreated]
    (addItemFormWidget, enctype) <- generateFormPost addItemForm

    -- if “new” cookie is set to 1, set isNew to True
    newCookie <- lookupCookie "new"
    let isNew = case newCookie of
                     Just "1" -> True
                     _        -> False

    -- delete “new” cookie
    when isNew $ deleteCookie "new" "/"

    let isEditView = True
    let feedid = queueShareId queue
    let feedinfo = $(widgetFile "feedinfo")
    let itemlist = $(widgetFile "itemlist")

    defaultLayout $ do
        setTitle $ toHtml $ queueTitle queue `append` " – RSSQueue"
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
             jsonToRepJson (i,unKey item)
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
