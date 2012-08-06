{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.New where

import Import

newtype NewFeedTitle = NewFeedTitle Text deriving Show

feedTitleForm :: Html -> MForm App App (FormResult NewFeedTitle, Widget)
feedTitleForm = renderDivs $ NewFeedTitle
    <$> areq textField "Name" Nothing

getNewR :: Handler RepHtml
getNewR = do
    (widget, enctype) <- generateFormPost feedTitleForm
    defaultLayout [whamlet|
        <h2>New Feed
        <form method=post action=@{NewR} enctype=#{enctype}>
            ^{widget}
            <input type=submit value="Continue →">
    |]

postNewR :: Handler RepHtml
postNewR = do
    ((result, _), _) <- runFormPost feedTitleForm

    case result of
         FormSuccess (NewFeedTitle title) -> do
            -- create a six-character, random identifier for the new feed
            ident <- liftIO $ createIdentifier 6
            -- insert into database and redirect to feed page
            _ <- runDB $ insert $ Feed ident title
            redirect (EditR ident)
         -- on errors, simply redirect to “new”
         _ -> redirect (NewR)
