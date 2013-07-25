import GpsModule
import RangeFinderModule
import Control.Concurrent

channelReadLoop channel = do
    message <- readChan channel
    _ <- putStrLn (show message)
    channelReadLoop channel

rangeReadLoop channel = do
    message <- readChan channel
    _ <- putStrLn (show message)
    rangeReadLoop channel

main = do              
        rmcChannel <- newChan
        rangeChannel <- newChan
        let gpsChannel = GpsModuleChannels rmcChannel
        _ <- startGpsModule gpsChannel
        _ <- startRangeFinderModule rangeChannel
        _ <- forkIO $ rangeReadLoop rangeChannel
        channelReadLoop rmcChannel
