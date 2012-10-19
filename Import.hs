{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Settings.StaticFiles
    , module Settings.Development
    , module Data.Monoid
    , module Control.Applicative
    , Text
    , urlBootstrapJs
    , getIpAddr
    , createIdentifiers
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
import Text.Julius (ToJavascript, toJavascript)
import Text.Blaze (ToMarkup, toMarkup)

import Settings.StaticFiles
import Settings.Development

instance ToJavascript Identifier where
    toJavascript (Identifier t) = toJavascript t

instance ToMarkup Identifier where
    toMarkup (Identifier t) = toMarkup t

-- central bootstrap url
urlBootstrapJs :: a -> Either (Route a) Text
urlBootstrapJs _ = Right "//netdna.bootstrapcdn.com/twitter-bootstrap/2.1.0/js/bootstrap.min.js"

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

createIdentifiers :: Int -> IO (Identifier,Identifier)
createIdentifiers len = do
    g <- getStdGen
    let str = take (2*len) . filter isAlphaNum . map chr $ randomRs (ord '0', ord 'z') g
    return (Identifier . pack $ take len str, Identifier .pack $ drop len str)

