module Handler.Help where

import Import

getHelpR :: Handler RepHtml
getHelpR = defaultLayout $ do
    setTitle "Help – RSSQueue"
    $(widgetFile "help")
