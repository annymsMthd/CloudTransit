{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import TestImport
import GpsTest

main :: IO ()
main = do    
    _ <- hspec gpsSpecs 
    _ <- hspec gpsUncmprss
