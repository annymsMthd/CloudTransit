{-# LANGUAGE OverloadedStrings #-}
module HomeTest
    ( homeSpecs
    ) where

import TestImport
import qualified Data.List as L

homeSpecs :: Spec
homeSpecs =
    ydescribe "Testing the basic landing page" $ do

        yit "should have the company name in the h1" $ do
            get HomeR
            statusIs 200
            htmlAllContain "h1" "Crowd Sourcing Public Transit"
