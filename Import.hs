{-# Language ScopedTypeVariables #-}
module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Settings.StaticFiles
    , module Settings.Development
    , module Data.Monoid
    , module Control.Applicative
    , Text
    , getIpAddr
    , createIdentifier
    , getResponseHeaders
    ) where

import Prelude hiding (writeFile, readFile, head, tail, init, last)
import Yesod   hiding (Route(..))
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Network.Socket (SockAddr (..))
import System.Random
import Data.Text (Text, pack, unpack)
import Data.Char
import Network.HTTP hiding (Request)
import qualified Network.HTTP as HTTP
import Network.URI

import Settings.StaticFiles
import Settings.Development

-- Utility functions

-- get an IP address from a socket address as text
getIpAddr :: SockAddr -> Text
getIpAddr (SockAddrInet   _ addr)     = (pack . show) addr
getIpAddr (SockAddrInet6 _ _ addr _ ) = (pack . show) addr
getIpAddr _                           = pack ""

getResponseHeaders :: Text -> IO (Maybe [HTTP.Header])
getResponseHeaders url =
  case parseURI url' of
       Nothing -> return Nothing
       Just u  -> do
           response <- simpleHTTP (mkRequest HEAD u)
           case response of
                Left _ -> return Nothing
                Right (Response _ _ hs (_::String)) -> return $ Just hs
    where url' = unpack url

createIdentifier :: Int -> IO Identifier
createIdentifier len = do
    g <- getStdGen
    let str = take len . filter isAlphaNum . map chr $ randomRs (ord '0', ord 'z') g
    return $ Identifier $ pack str

