{-# LANGUAGE OverloadedStrings #-}
module NewTest
    ( newSpecs
    ) where

import TestImport

newSpecs :: Specs
newSpecs =
  describe "This is a simple test for queue creation" $
    it "tries to create a new queue and makes sure it succeeds" $ do
      get_ "/new"
      statusIs 200
