import GpsModule
import Control.Concurrent

channelReadLoop channel = do
    message <- readChan channel
    _ <- putStrLn (show message)
    channelReadLoop channel

main = do              
        rmcChannel <- newChan
        let gpsChannel = GpsModuleChannels rmcChannel
        _ <- startGpsModule gpsChannel
        channelReadLoop rmcChannel
