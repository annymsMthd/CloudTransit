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

processMessage message = 
    putStrLn (show message)

processLine continuation buffer byt 
    | B.elem 10 byt = 
                do
                    _ <- processMessage buffer
                    continuation B.empty
    | B.elem 13 byt = continuation buffer
    | B.elem 36 byt = continuation buffer
    | otherwise = continuation $ B.append buffer byt

receiveLine serialPort buffer = 
                    do
                        byt <- recv serialPort 1
                        processLine (receiveLine serialPort) buffer byt

serialHandlerFunction port = receiveLine port B.empty

startGpsModule = do
        mybrick <- openSerial "/dev/ttyO4" settings
        serialHandlerFunction mybrick
