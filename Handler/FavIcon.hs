{-# LANGUAGE OverloadedStrings #-}

module Handler.FavIcon where

import Import

getFavIconR :: Int -> GHandler s m ()
getFavIconR 128 = sendFile "image/png" "static/img/favicon128.png"
getFavIconR 64  = sendFile "image/png" "static/img/favicon64.png"
getFavIconR 32  = sendFile "image/png" "static/img/favicon32.png"
getFavIconR _   = notFound
