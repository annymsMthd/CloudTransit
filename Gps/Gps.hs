module Gps where

import GpsTypes

earthRadius = 6371

toRadians degrees = pi / 180 * degrees

toDegrees radian = 180 / pi * radian

sin2 radian = sin radian ^ 2    

distance :: GpsPosition -> GpsPosition -> Double
distance pos1 pos2 = earthRadius * c
    where dLat = lat2 - lat1
          dLon = toRadians $ longitude pos2 - longitude pos1
          lat1 = toRadians . latitude $ pos1
          lat2 = toRadians . latitude $ pos2
          a = sin2 (dLat/2) + cos lat1 * cos lat2 * sin2 (dLon/2)
          c = 2 * atan2 (sqrt a) (sqrt $ 1-a)

bearing :: GpsPosition -> GpsPosition -> Double
bearing pos1 pos2 = toDegrees $ atan2 y x
    where dLon = toRadians $ longitude pos2 - longitude pos1
          lat1 = toRadians . latitude $ pos1
          lat2 = toRadians . latitude $ pos2
          y = sin dLon * cos lat2
          x = cos lat1 * sin lat2 - sin lat1 * cos lat2 * cos dLon

positionDifference :: GpsPosition -> GpsPosition -> PositionDifference
positionDifference gps1 gps2 = PositionDifference dist heading timeDifference
    where dist = distance gps1 gps2
          heading  = bearing gps1 gps2
          timeDifference = time gps2 - time gps1

compressPositions :: [GpsPosition] -> [PositionDifference]
compressPositions positions =
        [positionDifference (fst difference) (snd difference) | difference <- zip positions $ tail positions]
    
