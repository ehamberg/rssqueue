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
    items <- runDB $ selectList [FeedItemFeedId ==. feedId] [] >>= mapM (\(Entity _ v) -> return v)
    defaultLayout $ do
        setTitle $ toHtml $ feedTitle feed
        [whamlet|
            <h2>#{feedTitle feed}
            <ul>
              $forall feed_item <- items
                <li><a href="#{feedItemUri feed_item}">#{feedItemTitle feed_item}</a>
        |]
