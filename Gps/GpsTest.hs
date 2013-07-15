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
                     let tD = timeDif $ head combined
                     (assertBool ("time difference " ++ show tD ++ " should be 20") $ tD == 20)
                     (assertBool ("distance " ++ show dist ++ " shoud be 915.2814854138471") $ 
                        dist == 915.2814854138471)
                     (assertBool ("velocity " ++ show vel ++ " should be 45.764074270692355") $ 
                        vel == 45.764074270692355)
                     (assertBool ("heading " ++ show heading ++ " should be 89.50240746497025") $ 
                        heading == 89.50240746497025)

test2 = TestCase $ do
                     let gps1 = GpsPosition 34.064828 (-118.361379) 0
                     let gps2 = GpsPosition 34.064899 (-118.351455) 20
                     let gps3 = GpsPosition 34.064877 (-118.349330) 40
                     let gps4 = GpsPosition 34.065201 (-118.349357) 60
                     let gps5 = GpsPosition 34.066196 (-118.350564) 100
                     let path = gps1:gps2:gps3:gps4:gps5:[];
                     let combined = compressPositions $ path
                     let umcom = uncompressPositions [gps1] combined
                     let diff = [positionDifference (fst diff) (snd diff) | diff <- zip umcom path]
                     (assertBool "should have 2 gps positions" $ length umcom == 5)
                     (assertBool "should be the same gps positions" (all (<0.00000001) [distanceD d | d <- diff]))
                     (assertEqual "should be the same times" [time d | d <- path] [time d | d <- umcom])

tests = TestList [TestLabel "combining two gps positions" test1, TestLabel "uncompressing list" test2]

main = 
    do        
        runTestTT tests
