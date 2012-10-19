{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.New where

import Import
import Web.Cookie

newtype NewQueueTitle = NewQueueTitle Text deriving Show

-- length of identifiers for feeds
identifierLength :: Int
identifierLength = 5

feedTitleForm :: Html -> MForm RSSQueueApp RSSQueueApp (FormResult NewQueueTitle, Widget)
feedTitleForm = renderBootstrap $ NewQueueTitle
    <$> areq textField "Name" Nothing

getNewR :: Handler RepHtml
getNewR = do
    (widget, enctype) <- generateFormPost feedTitleForm
    defaultLayout $ $(widgetFile "new")

postNewR :: Handler RepHtml
postNewR = do
    ((result, _), _) <- runFormPost feedTitleForm

    case result of
         FormSuccess (NewQueueTitle title) -> do
            -- create two six-character identifiers for the queue – one secret
            -- and one to use if the user wishes to make the queue public
            (secretId,shareId) <- liftIO $ createIdentifiers identifierLength

            -- insert into database and redirect to edit page
            _ <- runDB $ insert $ Queue secretId shareId title False
            setCookie $ parseSetCookie "new = 1; max-age = 1; path = /"
            redirect (EditR secretId)
         -- on errors, simply redirect to “new”
         _ -> redirect NewR
