{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.New where

import Import

getNewR :: Handler RepHtml
getNewR = do
    -- create a six-character, random identifier for the new feed
    ident <- liftIO $ createIdentifier 6
    _ <- runDB $ insert $ Feed ident "My RSS Queue"
    redirect (FeedR ident)
