module GpsModule( startGpsModule ) where

import qualified Data.ByteString as B
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

receiveLine serialPort buffer = 
                    do
                        byt <- recv serialPort 1
                        if B.elem 10 byt then
                            do
                                receiveLine serialPort B.empty
                        else if B.elem 13 byt then
                            do
                                receiveLine serialPort buffer
                        else if B.elem 36 byt then
                            do
                                receiveLine serialPort buffer
                        else 
                            do
                                receiveLine serialPort $ B.append buffer byt

serialHandlerFunction port = receiveLine port B.empty

startGpsModule = do
        mybrick <- openSerial "/dev/ttyO4" settings
        serialHandlerFunction mybrick
