{-# LANGUAGE OverloadedStrings #-}

module Handler.TouchIcon where

import Import

getTouchIconR :: Int -> GHandler s m ()
getTouchIconR 144 = sendFile "image/png" "static/img/touch-icon-144x144.png"
getTouchIconR 114 = sendFile "image/png" "static/img/touch-icon-114x114.png"
getTouchIconR 72  = sendFile "image/png" "static/img/touch-icon-72x72.png"
getTouchIconR 57  = sendFile "image/png" "static/img/touch-icon-57x57.png"
getTouchIconR _   = notFound
