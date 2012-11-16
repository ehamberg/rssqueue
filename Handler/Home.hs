{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Form.Jquery (urlJqueryJs)

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    lift getYesod >>= (addScriptEither . urlJqueryJs)
    lift getYesod >>= (addScriptEither . urlBootstrapJs)
    aDomId <- lift newIdent
    setTitle "RSS Queue"
    $(widgetFile "homepage")
