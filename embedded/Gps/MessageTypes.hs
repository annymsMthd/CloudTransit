module Gps.MessageTypes ( module Gps.MessageTypes ) where

import Prelude
import Gps.GpsTypes

data FixQuality = Invalid | Gps | DGPS | PPS| RealTimeKinematic | 
                  FloatRtk | DeadReckoning | ManualInput | 
                  SimulationMode deriving (Enum, Eq, Show)

data Fix3d = NoFix | Fix2d | Fix3d deriving (Enum, Show, Eq)

data GGA = GGA { gaTime            :: Ms,
                 gaLatitude        :: Latitude,
                 gaLongitude       :: Longitude,
                 gaFixQuality      :: FixQuality,
                 gaSatelliteNumber :: Integer,
                 gaHorizontalDoP   :: Double,
                 gaAltitude        :: Mtrs,
                 gaHeightOfGeoId   :: Mtrs } deriving (Show, Eq)

data GSA = GSA { saAutoSelection   :: Bool,
                 saFix3D           :: Fix3d,
                 saPrnOfSatellites :: [Integer],
                 saPdop            :: Double,
                 saHdop            :: Double,
                 saVdop            :: Double } deriving (Show, Eq)

data GSV = GSV { svNumberOfSentences :: Integer,
                 svSentenceNumber    :: Integer,
                 svSatelliteNumber   :: Integer,
                 svElevation         :: Degrees,
                 svAzimuth           :: Degrees,
                 svSnr               :: Integer } deriving (Show, Eq)

data RMC = RMC { mcTime              :: Ms,
                 mcActive            :: Bool,
                 mcLatitude          :: Latitude,
                 mcLongitude         :: Longitude,                 
                 mcSpeed             :: Double,
                 mcTrackAngle        :: Double,
                 mcDate              :: Double,
                 mcMagneticVariation :: Double } deriving (Show, Eq)

data GLL = GLL { llLatitude  :: Latitude,
                 llLongitude :: Longitude,
                 llTime      :: Ms,
                 llActive    :: Bool } deriving (Show, Eq)

data VTG = VTG { tgTrueTackMadeGood      :: Degrees,
                 tgMagneticTrackMadeGood :: Degrees,
                 tgGroundSpeedKnots      :: Double,
                 tgGroundSpeedKm         :: Double } deriving (Show, Eq)
