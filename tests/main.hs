{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import Settings
import Yesod.Default.Config
import Yesod.Test
import Application (makeFoundation)

import Database.Persist.GenericSql.Raw (withStmt)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Database.Persist.GenericSql (runSqlPool)

import HomeTest
import NewTest
import EditTest

main :: IO ()
main = do
    conf <- loadConfig $ (configSettings Testing) { csParseExtra = parseExtra }
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation

    let dropTestQueue = do
            let sql = "DELETE FROM Queue"
            C.runResourceT $ withStmt sql [] C.$$ CL.mapM_ $ liftIO . print
    let createTestQueue = do
            let sql = "INSERT INTO Queue VALUES(NULL, 'xxxxx', 'Test Queue')"
            C.runResourceT $ withStmt sql [] C.$$ CL.mapM_ $ liftIO . print

    runSqlPool (dropTestQueue) (connPool foundation)
    runSqlPool (createTestQueue) (connPool foundation)

    runTests app (connPool foundation) homeSpecs
    runTests app (connPool foundation) newSpecs
    runTests app (connPool foundation) editSpecs
