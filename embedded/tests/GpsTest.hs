{-# LANGUAGE OverloadedStrings #-}
module GpsTest 
    ( module GpsTest
    ) where

import TestImport 
import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Control.Exception (evaluate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

gpsSpecs = describe "Gps Tests" $ do
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
                it "should be the same times" $ [time d | d <- path] `shouldBe` [time d | d <- umcom]

ggaMessageTypeConversion = describe "Should convert bytestring to gga gps message" $ do
                let bytestringArray = [ C.pack "109.100", 
                                        C.pack "4807.038", C.pack "N",
                                        C.pack "01131.000", C.pack "E",
                                        C.pack "1",
                                        C.pack "12",
                                        C.pack "0.9",
                                        C.pack "100", C.pack "M",
                                        C.pack "10000", C.pack "M"]
                let gga = convertToGGA bytestringArray
                _ <- it ("gga time " ++ show (gaTime gga) ++ " should be 109100") $ 
                        gaTime gga == 109100
                _ <- it ("gga latitude " ++ show (gaLatitude gga) ++ " should be 48.11729999999999") $ 
                        gaLatitude gga == 48.11729999999999
                _ <- it ("gga longitude " ++ show (gaLongitude gga) ++ " should be 11.516666666666667") $ 
                        gaLongitude gga == 11.516666666666667
                _ <- it ("gga fix quality " ++ show (gaFixQuality gga) ++ " should be GpsFix") $ 
                        gaFixQuality gga == Gps
                _ <- it ("gga number of satellites " ++ show (gaSatelliteNumber gga) ++ " should be 12") $ 
                        gaSatelliteNumber gga == 12
                _ <- it ("gga horizontal dilution of position " ++ show (gaHorizontalDoP gga) ++ " should be 0.9") $ 
                        gaHorizontalDoP gga == 0.9
                _ <- it ("gga altitude " ++ show (gaAltitude gga) ++ " should be 100") $ 
                        gaAltitude gga == 100
                it ("gga geod height " ++ show (gaHeightOfGeoId gga) ++ " should be 10000") $ 
                        gaHeightOfGeoId gga == 10000

gsaMessageTypeConversion = describe "Should convert ByteString to gsa message" $ do
                let byteStringArray = [ C.pack "A",
                                        C.pack "1",
                                        C.pack "",
                                        C.pack "",
                                        C.pack "",
                                        C.pack "",
                                        C.pack "",
                                        C.pack "",
                                        C.pack "",
                                        C.pack "",
                                        C.pack "",
                                        C.pack "",
                                        C.pack "",
                                        C.pack "",
                                        C.pack "1.2",
                                        C.pack "2.3",
                                        C.pack "3.2" ]
                let gsa = convertToGSA byteStringArray
                _ <- it ("gsa AutoSelection " ++ show (saAutoSelection gsa) ++ " should be true") $ 
                        saAutoSelection gsa
                _ <- it ("gsa Fix3D " ++ show (saFix3D gsa) ++ " should be 2d") $ 
                        saFix3D gsa == Fix2d
                _ <- it ("gsa pdop " ++ show (saPdop gsa) ++ " should be 1.2") $ 
                        saPdop gsa == 1.2
                _ <- it ("gsa hdop " ++ show (saHdop gsa) ++ " should be 2.3") $ 
                        saHdop gsa == 2.3
                it ("gsa vdop " ++ show (saVdop gsa) ++ " should be 3.2") $ 
                        saVdop gsa == 3.2

rmcMessageTypeConversion = describe "Should convert bytestring to rmc gps message" $ do
                let bytestringArray = [ C.pack "123519", 
                                        C.pack "A",
                                        C.pack "4807.038", C.pack "N",
                                        C.pack "01131.000", C.pack "E",
                                        C.pack "022.4",
                                        C.pack "084.4",
                                        C.pack "230394",
                                        C.pack "003.1", C.pack "W"]
                let gga = convertToRMC bytestringArray
                _ <- it ("gga time " ++ show (mcTime gga) ++ " should be 123519") $ 
                        mcTime gga == 123519000
                _ <- it ("gmc status " ++ show (mcActive gga) ++ " should be True") $
                        mcActive gga
                _ <- it ("gga latitude " ++ show (mcLatitude gga) ++ " should be 48.11729999999999") $ 
                        mcLatitude gga == 48.11729999999999
                _ <- it ("gga longitude " ++ show (mcLongitude gga) ++ " should be 11.516666666666667") $ 
                        mcLongitude gga == 11.516666666666667
                _ <- it ("gga speed " ++ show (mcSpeed gga) ++ " should be 22.4") $ 
                        mcSpeed gga == 22.4
                _ <- it ("gga track angle " ++ show (mcTrackAngle gga) ++ " should be 84.4") $ 
                        mcTrackAngle gga == 84.4
                _ <- it ("gga date " ++ show (mcDate gga) ++ " should be 230394") $ 
                        mcDate gga == 230394
                it ("gga magnetic variation " ++ show (mcMagneticVariation gga) ++ " should be 3.1") $ 
                        mcMagneticVariation gga == 3.1
                

runGpsTests = do
            _ <- hspec gpsSpecs 
            _ <- hspec gpsUncmprss
            _ <- hspec ggaMessageTypeConversion
            _ <- hspec gsaMessageTypeConversion
            _ <- hspec rmcMessageTypeConversion
            putStrLn "finished gps tests"
