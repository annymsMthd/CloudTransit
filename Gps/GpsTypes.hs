module GpsTypes where

type Latitude = Double
type Longitude = Double
type TimeMs = Int

data GpsPosition = 
     GpsPosition { latitude  :: Latitude,
                   longitude :: Longitude,
                   time      :: TimeMs }

data PositionDifference =
     PositionDifference { distanceD :: Double,
                          bearingD  :: Double,
                          timeDif   :: TimeMs }
