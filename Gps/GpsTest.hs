import GpsTypes
import Gps
import Test.HUnit

test1 = TestCase $ do
                     let gps1 = GpsPosition 34.064828 (-118.361379) 0
                     let gps2 = GpsPosition 34.064899 (-118.351455) 20                    
                     let combined = compressPositions $ gps1:[gps2]
                     let dist = distanceD $ head combined
                     let heading = bearingD $ head combined
                     let vel = velocity $ head combined
                     (assertBool "time difference is correct" $ timeDif (head combined) == 20)
                     (assertBool ("distance " ++ show dist ++ " shoud be 0.9141766102139338") $ 
                        dist == 0.9141766102139338)
                     (assertBool ("velocity " ++ show vel ++ " should be 4.570883051069669e-2") $ vel == 4.570883051069669e-2)
                     (assertBool ("heading " ++ show heading ++ " should be 89.50240746497025") $ heading == 89.50240746497025)

test2 = TestCase $ do
                     let gps1 = GpsPosition 34.064828 (-118.361379) 0
                     let gps2 = GpsPosition 34.064899 (-118.351455) 20
                     let gps3 = GpsPosition 34.064877 (-118.349330) 40
                     let gps4 = GpsPosition 34.065201 (-118.349357) 60
                     let gps5 = GpsPosition 34.066196 (-118.350564) 100
                     let path = gps1:gps2:gps3:gps4:gps5:[];
                     let combined = compressPositions $ path
                     let umcom = uncompressPositions [gps1] combined
                     (assertBool "should have 2 gps positions" $ length umcom == 2)
                     (assertEqual "should be the same gps positions" path umcom)

tests = TestList [TestLabel "combining two gps positions" test1, TestLabel "uncompressing list" test2]
