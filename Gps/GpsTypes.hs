module GpsTypes where

type Latitude = Double
type Longitude = Double
type Ms = Double
type KmPerMs = Double
type Km = Double

data GpsPosition = 
     GpsPosition { latitude  :: Latitude,
                   longitude :: Longitude,
                   time      :: Ms }

data PositionDifference =
     PositionDifference { distanceD :: Km,
                          bearingD  :: Double,
                          timeDif   :: Ms,
                          velocity  :: KmPerMs }
