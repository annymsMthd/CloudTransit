module RangeFinderModule where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import System.Hardware.Serialport
import System.IO
import Data.Word
import Data.Bits
import Data.Hex
import Control.Concurrent

data RangeMessage = RangeMessage { range :: Integer } deriving (Show)

settings = SerialPortSettings CS9600 8 One NoParity NoFlowControl 1000

processMessage message channel = do 
    writeChan channel (RangeMessage (read $ C.unpack message))

receive serialPort channel = do
                        byt <- recv serialPort 1
                        if B.elem 82 byt then do 
                            message1 <- recv serialPort 1
                            message2 <- recv serialPort 1
                            message3 <- recv serialPort 1
                            message4 <- recv serialPort 1
                            _ <- processMessage (C.append (C.append (C.append message1 message2) message3) message4) channel
                            receive serialPort channel
                        else receive serialPort channel

serialHandlerFunction channel = do
                port <- openSerial "/dev/ttyO2" settings
                _ <- flush port
                forkIO $ receive port channel

startRangeFinderModule channel = do
                serialHandlerFunction channel
