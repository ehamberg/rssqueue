{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    let welcomeWidget = $(widgetFile "welcome")
    aDomId <- lift newIdent
    setTitle "RSS Queue"
    $(widgetFile "homepage")
