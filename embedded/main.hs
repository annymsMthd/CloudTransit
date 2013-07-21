import GpsModule
import Control.Concurrent

main = do
        forkIO $ do
            startGpsModule
        _ <- getLine
        putStrLn "test"
