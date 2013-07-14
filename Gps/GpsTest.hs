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

tests = TestList [TestLabel "combining two gps positions" test1]
