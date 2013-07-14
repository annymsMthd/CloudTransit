module GpsTypes where

type Latitude = Double
type Longitude = Double
type TimeMs = Double
type KmPerMs = Double
type Km = Double

data GpsPosition = 
     GpsPosition { latitude  :: Latitude,
                   longitude :: Longitude,
                   time      :: TimeMs }

data PositionDifference =
     PositionDifference { distanceD :: Km,
                          bearingD  :: Double,
                          timeDif   :: TimeMs,
                          velocity  :: KmPerMs }
