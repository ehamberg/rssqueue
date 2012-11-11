module Handler.About where

import Import

getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
  setTitle "About RSSQueue"
  $(widgetFile "about")
