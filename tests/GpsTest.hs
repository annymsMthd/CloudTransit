{-# LANGUAGE OverloadedStrings #-}
module GpsTest 
    ( gpsSpecs
    ) where

import TestImport 
import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Control.Exception (evaluate)

gpsSpecs = _ <- describe "Gps Tests" $ do
                let gps1 = GpsPosition 34.064828 (-118.361379) 0
                let gps2 = GpsPosition 34.064899 (-118.351455) 20                    
                let combined = compressPositions $ gps1:[gps2]
                let dist = distanceD $ head combined
                let heading = bearingD $ head combined
                let vel = velocity $ head combined
                let tD = timeDif $ head combined
                _ <- it ("time difference " ++ show tD ++ " should be 20") $                      
                    tD == 20
                _ <- it ("distance " ++ show dist ++ " shoud be 915.2814854138471") $ 
                    dist == 915.2814854138471
                _ <- it ("velocity " ++ show vel ++ " should be 45.764074270692355") $ 
                    vel == 45.764074270692355
                it ("heading " ++ show heading ++ " should be 89.50240746497025") $ 
                    heading == 89.50240746497025

gpsUncmprss = describe "Compress Gps Points" $ do
                let gps1 = GpsPosition 34.064828 (-118.361379) 0
                let gps2 = GpsPosition 34.064899 (-118.351455) 20
                let gps3 = GpsPosition 34.064877 (-118.349330) 40
                let gps4 = GpsPosition 34.065201 (-118.349357) 60
                let gps5 = GpsPosition 34.066196 (-118.350564) 100
                let path = gps1:gps2:gps3:gps4:gps5:[]
                let combined = compressPositions $ path
                let umcom = uncompressPositions [gps1] combined
                let diff = [positionDifference (fst diff) (snd diff) | diff <- zip umcom path]
                _ <- it "should have 2 gps positions" $ length umcom == 5
                _ <- it "should be the same gps positions" $ all (<0.00000001) [distanceD d | d <- diff]
                it "should be the same times" [time d | d <- path] == [time d | d <- umcom]

--test3 = TestCase $ do
                   --  let gps1 = GpsPosition 34.064828 (-118.361379) 0
                   --  let gps2 = GpsPosition 34.064899 (-118.351455) 20
                   --  let gps3 = GpsPosition 34.064877 (-118.349330) 40
                   --  let gps4 = GpsPosition 34.065201 (-118.349357) 60
                   --  let gps5 = GpsPosition 34.066196 (-118.350564) 100
                   --  let path = gps1:gps2:gps3:gps4:gps5:[]
                   --  let headingMap = getHeadingMapList (zip path (tail path)) 0
                    -- (assertBool ("heading map" ++ show headingMap ++ " should have 5") $ length headingMap == 6)

--gpsTests = TestList [
                 --   TestLabel "combining two gps positions" test1, 
                 --   TestLabel "uncompressing list" test2,
                --   TestLabel "headingMaps" test3]
        
