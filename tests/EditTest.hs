{-# LANGUAGE OverloadedStrings #-}
module EditTest
    ( editSpecs
    ) where

import TestImport

editSpecs :: Specs
editSpecs =
  describe "This is a feed editing test" $
    it "it loads the example queue and make sure it can add items" $ do
      get_ "/edit/xxxxx"
      statusIs 200
      --htmlAllContain "h1" "Hello"

      --statusIs 200
      --htmlCount ".message" 1
      --htmlAllContain ".message" "Some Content"
      --htmlAllContain ".message" "text/plain"
