module Handler.About where

import Import
import Yesod.Form.Jquery (urlJqueryJs)

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    lift getYesod >>= (addScriptEither . urlJqueryJs)
    lift getYesod >>= (addScriptEither . urlBootstrapJs)
    setTitle "About RSSQueue"
    $(widgetFile "about")
