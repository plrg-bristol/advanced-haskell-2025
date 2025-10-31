module Concurrency where

import Control.Concurrent
-- import Control.Concurrent.Chan
import Control.Monad
import Data.Maybe

readWordsChan :: FilePath -> IO (Chan (Maybe String))
readWordsChan path = do
    f <- readFile path
    c <- newChan
    tid <- forkIO $ do
        forM_ (words f) $ \w -> do
            writeChan c (Just w)
            threadDelay 1000000
            -- threadDelay 10
        writeChan c Nothing
    -- killThread tid
    pure c

main :: IO ()
main = do
    c <- readWordsChan "../README.md"
    -- w <- readChan c
    ws <- getChanContents c
    forM_ (takeWhile isJust ws) $ \w -> do
        case w of
            Just x -> putStrLn (reverse x)
            Nothing -> undefined

-- STM?
-- after game of life, can we make it concurrent?