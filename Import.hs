{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Import
    ( module Import
    ) where

import Prelude as Import hiding (writeFile, readFile, head, tail, init, last)
import Yesod as Import hiding (Route (..))
import Control.Applicative  as Import (pure, (<$>), (<*>))
import Data.Monoid as Import (Monoid (mappend, mempty, mconcat), (<>))
import Data.Text as Import (Text)
import Foundation as Import
import Model as Import
import Settings as Import
import Settings.Development as Import
import Settings.StaticFiles as Import
import Network.Socket (SockAddr (..))
import System.Random
import Data.Text (pack, unpack)
import Data.Text.Lazy.Builder (fromText)
import Data.Char
import Network.HTTP hiding (Request, getRequest)
import Network.Wai (remoteHost)
import qualified Network.HTTP as HTTP
import Network.URI
import Text.Julius (ToJavascript, toJavascript)
import Text.Blaze (ToMarkup, toMarkup)

instance ToJavascript Identifier where
    toJavascript (Identifier t) = fromText t

instance ToMarkup Identifier where
    toMarkup (Identifier t) = toMarkup t

-- CSS properties
greybg :: Text
greybg = "rgb(249,249,249)"

-- Utility functions

-- get an IP address from a socket address as text
getIpAddr :: GHandler s m Text
getIpAddr = fmap (getAddr . remoteHost . reqWaiRequest) getRequest
  where getAddr (SockAddrInet   _ addr)     = (pack . show) addr
        getAddr (SockAddrInet6 _ _ addr _ ) = (pack . show) addr
        getAddr _                           = pack ""

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
  let str = take (2*len) . filter idChar . map chr $ randomRs (ord '0', ord 'z') g
  return (Identifier . pack $ take len str, Identifier .pack $ drop len str)
    where idChar c = isAlphaNum c && c `notElem` "0OI1l"
