module Gps where

import GpsTypes
import Data.Fixed

earthRadius = 6371

toRadians degrees = pi / 180 * degrees

toDegrees radian = 180 / pi * radian

sin2 radian = sin radian ^ 2    

getVelocity :: Km -> Ms -> KmPerMs
getVelocity dist time = dist / time
 
distance :: GpsPosition -> GpsPosition -> Km
distance pos1 pos2 = earthRadius * c
    where dLat = lat2 - lat1
          dLon = toRadians $ longitude pos2 - longitude pos1
          lat1 = toRadians $ latitude pos1
          lat2 = toRadians $ latitude pos2
          a = sin2 (dLat/2) + cos lat1 * cos lat2 * sin2 (dLon/2)
          c = 2 * atan2 (sqrt a) (sqrt $ 1-a)

bearing :: GpsPosition -> GpsPosition -> Double
bearing pos1 pos2 = mod' (brg + 360) 360
    where dLon = toRadians $ longitude pos2 - longitude pos1
          lat1 = toRadians $ latitude pos1
          lat2 = toRadians $ latitude pos2
          y = sin dLon * cos lat2
          x = cos lat1 * sin lat2 - sin lat1 * cos lat2 * cos dLon
          brg = 180 / pi * atan2 y x

destinationPoint :: GpsPosition -> PositionDifference -> GpsPosition
destinationPoint origin diff = GpsPosition (toDegrees lat2) (toDegrees lng2) tm
     where lat1 = toRadians $ latitude origin
           lng1 = toRadians $ longitude origin
           dR = distanceD diff / earthRadius
           bR = toRadians $ bearingD diff
           lat2 = asin $ sin lat1 * cos dR + cos lat1 * sin dR * cos bR
           lng2 = lng1 + atan2 (sin bR * sin dR * cos lat1) (cos dR - sin lat1 * sin lat2)
           tm = time origin + timeDif diff

positionDifference :: GpsPosition -> GpsPosition -> PositionDifference
positionDifference gps1 gps2 = PositionDifference dist heading timeDifference vel
    where dist = distance gps1 gps2
          heading  = bearing gps1 gps2
          timeDifference = time gps2 - time gps1
          vel = getVelocity dist timeDifference

compressPositions :: [GpsPosition] -> [PositionDifference]
compressPositions positions =
        [positionDifference (fst difference) (snd difference) | difference <- zip positions $ tail positions]

uncompressPositions :: [GpsPosition] -> [PositionDifference] -> [GpsPosition]
uncompressPositions origin (y:mr) = uncompressPositions (origin ++ [destinationPoint (last origin) y]) mr
uncompressPositions origin [] = origin
        

    
