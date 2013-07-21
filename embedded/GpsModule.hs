module GpsModule( startGpsModule ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import System.Hardware.Serialport
import System.IO
import Data.Word
import Data.Bits
import Data.Hex

settings = SerialPortSettings CS9600 8 One NoParity NoFlowControl 10

validateCheckSum strng = checksum == (hex $ B.pack [strngChecksum])
    where break =  B.splitWith (==42) strng
          checksum = last break
          strngChecksum = B.foldr (xor) 0 $ head break

convertToMessageType message
    | messageType == "GPGGA" = do putStrLn "Global Positioning System Fix Data"
    | messageType == "GPGSA" = do putStrLn "Satellite status"
    | messageType == "GPGSV" = do putStrLn "Satellites in view"
    | messageType == "GPRMC" = do putStrLn "Recommended Minimum sentence C"
    | messageType == "GPGLL" = do putStrLn "Geographic position, Latitude and Longitude"
    | messageType == "GPVTG" = do putStrLn "Track made good and ground speed"
    | otherwise = do putStrLn "unknown type"
    where frags = B.splitWith(==44) message
          messageType = C.unpack $ head frags

processMessage message  
    | validateCheckSum message = convertToMessageType message
    | otherwise = do putStrLn "invalid message"

processLine continuation buffer byt 
    | B.elem 10 byt = do
                    _ <- processMessage buffer
                    continuation B.empty
    | B.elem 13 byt = continuation buffer
    | B.elem 36 byt = continuation buffer
    | otherwise = continuation $ B.append buffer byt

receive serialPort buffer = do
                        byt <- recv serialPort 1
                        processLine (receive serialPort) buffer byt

serialHandlerFunction port = receive port B.empty

startGpsModule = do
        mybrick <- openSerial "/dev/ttyO4" settings
        serialHandlerFunction mybrick
