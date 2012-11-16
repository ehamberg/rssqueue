module Handler.Help where

import Import
import Yesod.Form.Jquery (urlJqueryJs)

getHelpR :: Handler RepHtml
getHelpR = defaultLayout $ do
    lift getYesod >>= (addScriptEither . urlJqueryJs)
    lift getYesod >>= (addScriptEither . urlBootstrapJs)
    setTitle $ "Help – RSSQueue"
    $(widgetFile "help")
