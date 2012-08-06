{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.New where

import Import

newtype NewQueueTitle = NewQueueTitle Text deriving Show

feedTitleForm :: Html -> MForm App App (FormResult NewQueueTitle, Widget)
feedTitleForm = renderDivs $ NewQueueTitle
    <$> areq textField "Name" Nothing

getNewR :: Handler RepHtml
getNewR = do
    (widget, enctype) <- generateFormPost feedTitleForm
    defaultLayout [whamlet|
        <h2>New Queue
        <form method=post action=@{NewR} enctype=#{enctype}>
            ^{widget}
            <input type=submit value="Continue →">
    |]

postNewR :: Handler RepHtml
postNewR = do
    ((result, _), _) <- runFormPost feedTitleForm

    case result of
         FormSuccess (NewQueueTitle title) -> do
            -- create a six-character, random identifier for the new queue
            ident <- liftIO $ createIdentifier 6
            -- insert into database and redirect to edit page
            _ <- runDB $ insert $ Queue ident title
            redirect (EditR ident)
         -- on errors, simply redirect to “new”
         _ -> redirect (NewR)
