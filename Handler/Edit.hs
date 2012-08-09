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
import Data.Text (isPrefixOf, append, length, head, find)

instance ToJavascript Identifier where
    toJavascript (Identifier t) = toJavascript t

addItemForm :: Html -> MForm RSSQueueApp RSSQueueApp (FormResult (Text,Text), Widget)
addItemForm = renderDivs $ (,)
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

    items <- runDB $ selectList [QueueItemQueueId ==. key] [Desc QueueItemCreated] >>= mapM (\(Entity _ v) -> return v)
    (widget, enctype) <- generateFormPost addItemForm

    defaultLayout $ do
        lift getYesod >>= (addScriptEither . urlJqueryJs)
        addButtonId <- lift newIdent
        titleFieldId <- lift newIdent
        urlFieldId <- lift newIdent
        setTitle $ toHtml $ queueTitle queue
        $(widgetFile "edit")

postEditR :: Identifier -> Handler RepJson
postEditR identifier = do
    Just (Entity key _) <- runDB $ getBy $ UniqueIdentifier identifier

    ((result, _), _) <- runFormPost addItemForm

    case result of
         FormSuccess (title,url) -> do
             let url' = if "http://" `isPrefixOf` url || "https://" `isPrefixOf` url
                           then url
                           else "http://" `append` url
             time <- liftIO getCurrentTime
             ip <- fmap (getIpAddr . remoteHost . reqWaiRequest) getRequest
             _ <- runDB $ insert $ QueueItem key title url' time ip
             jsonToRepJson $ String "success"
         FormFailure errors -> do
             liftIO $ print errors
             jsonToRepJson $ String $ mconcat errors
         _ -> jsonToRepJson $ String "error"
