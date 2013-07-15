module GpsTypes where

type Latitude = Double
type Longitude = Double
type Ms = Double
type MtrsPerMs = Double
type Mtrs = Double

data GpsPosition = 
     GpsPosition { latitude  :: Latitude,
                   longitude :: Longitude,
                   time      :: Ms }
     deriving (Show, Eq)

data PositionDifference =
     PositionDifference { distanceD :: Mtrs,
                          bearingD  :: Double,
                          timeDif   :: Ms,
                          velocity  :: MtrsPerMs }
     deriving (Show, Eq)
