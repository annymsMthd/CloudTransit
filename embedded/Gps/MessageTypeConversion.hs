module Gps.MessageTypeConversion ( module Gps.MessageTypeConversion ) where

import Prelude
import Gps.GpsTypes
import Gps.MessageTypes
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List

timeDegrees :: (C.ByteString, C.ByteString) -> Double
timeDegrees time = n * (hr + (min / 60))
        where x = read (C.unpack $ fst time)
              hr = fromIntegral (round $ x / 100)
              min = x - (hr * 100)
              c = C.unpack $ snd time
              n
                | c == "S" = -1
                | c == "W" = -1
                | otherwise = 1

latLong :: [C.ByteString] -> Int -> Double
latLong lst i = if strng == "" then 0 else timeDegrees x
        where strng = C.unpack $ fst x 
              x = (lst !! i, lst !! (i + 1))

strnNum lst i = if x == "" then 0 else read x
    where x = C.unpack $ lst !! i

strnEnum lst i = toEnum (read $ C.unpack (lst !! i))

strnBool lst i chk = (C.unpack $ lst !! i) == chk

strnTime lst i = read (C.unpack $ lst !! i) * 1000

convertToGGA :: [B.ByteString] -> GGA
convertToGGA lst = GGA time lat lng fix satNum hop alt hgt 
    where time = strnTime lst 0
          lat = latLong lst 1
          lng = latLong lst 3
          fix = strnEnum lst 5
          satNum = strnNum lst 6
          hop = strnNum lst 7
          alt = strnNum lst 8
          hgt = strnNum lst 10

convertToGSA lst = GSA auto fix [] pdop hdop vdop 
    where auto = strnBool lst 0 "A"
          fix = strnEnum lst 1
          pdop = strnNum lst 14
          hdop = strnNum lst 15
          vdop = strnNum lst 16

convertToRMC lst = RMC time sts lat lng spd ang dt mv
    where time = strnTime lst 0
          sts = strnBool lst 1 "A"
          lat = latLong lst 2
          lng = latLong lst 4
          spd = strnNum lst 6
          ang = strnNum lst 7
          dt  = strnNum lst 8
          mv  = strnNum lst 9

convertToGLL lst = GLL lat lng time act
    where lat = latLong lst 0
          lng = latLong lst 2
          time = strnTime lst 4
          act = strnBool lst 5 "A"

convertToVTG lst = VTG tt mt gspdk gspdkm
    where tt = strnNum lst 0
          mt = strnNum lst 2
          gspdk = strnNum lst 4
          gspdkm = strnNum lst 6
