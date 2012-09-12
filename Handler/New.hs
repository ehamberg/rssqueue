{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.New where

import Import

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
            -- create a six-character, random identifier for the new queue
            ident <- liftIO $ createIdentifier identifierLength
            -- insert into database and redirect to edit page
            _ <- runDB $ insert $ Queue ident title
            redirect (EditR ident)
         -- on errors, simply redirect to “new”
         _ -> redirect NewR
