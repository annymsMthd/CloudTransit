module GpsModule( module GpsModule ) where

import Gps.MessageTypeConversion
import Gps.MessageTypes
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import System.Hardware.Serialport
import System.IO
import Data.Word
import Data.Bits
import Data.Hex
import Control.Concurrent

settings = SerialPortSettings CS9600 8 One NoParity NoFlowControl 10

data GpsModuleChannels = GpsModuleChannels { rmcChannel :: Chan RMC }

validateCheckSum strng = checksum == (hex $ B.pack [strngChecksum])
    where break =  B.splitWith (==42) strng
          checksum = last break
          strngChecksum = B.foldr (xor) 0 $ head break

convertToMessageType message channel
    -- | messageType == "GPGGA" = do putStrLn $ show (convertToGGA fragTail)
    -- | messageType == "GPGSA" = do putStrLn $ show (convertToGSA fragTail)
    -- | messageType == "GPGSV" = do putStrLn "Satellites in view"
    | messageType == "GPRMC" = do writeChan (rmcChannel channel) $ convertToRMC fragTail
    -- | messageType == "GPGLL" = do putStrLn $ show (convertToGLL fragTail)
    -- | messageType == "GPVTG" = do putStrLn $ show (convertToVTG fragTail)
    | otherwise = do return ()
    where frags = B.splitWith(==44) message
          messageType = C.unpack $ head frags
          fragTail = map (\x -> B.takeWhile (/=42) x) $ tail frags

processMessage message channel 
    | validateCheckSum message = convertToMessageType message channel
    | otherwise = do putStrLn "invalid message"

processLine continuation buffer byt channel
    | B.elem 10 byt = do
                    _ <- processMessage buffer channel
                    continuation B.empty channel
    | B.elem 13 byt = continuation buffer channel
    | B.elem 36 byt = continuation buffer channel
    | otherwise = continuation (B.append buffer byt) channel

receive serialPort buffer channel = do
                        byt <- recv serialPort 1
                        processLine (receive serialPort) buffer byt channel

serialHandlerFunction channel = do
                port <- openSerial "/dev/ttyO4" settings
                forkIO $ receive port B.empty channel

startGpsModule channel = do        
        serialHandlerFunction channel
