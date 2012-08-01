{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Feed where

import Import

-- This is a handler function for the GET request method on the FeedR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getFeedR :: FeedId -> Handler RepHtml
getFeedR feedId = do
    feed <- runDB $ get404 feedId
    urls <- runDB $ selectList [FeedItemFeedId ==. feedId] [LimitTo 3]
    defaultLayout $ do
        setTitle $ "My feed"
        [whamlet| <h2>#{feedTitle feed}
                  <p>#{show urls}
        |]
