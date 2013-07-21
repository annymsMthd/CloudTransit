import GpsModule
import Control.Concurrent

main = do
        forkIO $ do
            startGpsModule
        putStrLn "test"
