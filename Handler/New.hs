{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.New where

import Import

getNewR :: Handler RepHtml
getNewR = do
    feedId <- runDB $ insert $ Feed "My RSS Queue"
    redirect (FeedR feedId)
