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
    ) where

import Prelude hiding (writeFile, readFile, head, tail, init, last)
import Yesod   hiding (Route(..))
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Network.Socket (SockAddr (..))
import System.Random
import Data.Text (Text, pack)
import Data.Char

import Settings.StaticFiles
import Settings.Development

-- Utility functions

-- get an IP address from a socket address as text
getIpAddr :: SockAddr -> Text
getIpAddr (SockAddrInet   _ addr)     = (pack . show) addr
getIpAddr (SockAddrInet6 _ _ addr _ ) = (pack . show) addr
getIpAddr _                           = pack ""

createIdentifier :: Int -> IO Text
createIdentifier len = do
    g <- getStdGen
    let str = take len . filter isAlphaNum . map chr $ randomRs (ord '0', ord 'z') g
    return $ pack str

