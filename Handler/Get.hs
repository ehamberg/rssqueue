{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Get where

import Import

getGetR :: Text -> Handler RepHtml
getGetR = redirect
