{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.New where

import Import
import System.Random
import Data.Char
import Data.Text (pack)

createIdentifier :: Int -> IO Text
createIdentifier len = do
    g <- getStdGen
    let str = take len . filter isAlphaNum . map chr $ randomRs (ord '0', ord 'z') g
    return $ pack str

getNewR :: Handler RepHtml
getNewR = do
    ident <- liftIO $ createIdentifier 6
    feedId <- runDB $ insert $ Feed "My RSS Queue" ident
    redirect (FeedR feedId)
