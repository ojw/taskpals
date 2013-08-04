{-# LANGUAGE OverloadedStrings #-}

module Server where

import Network.WebSockets
import qualified Data.Aeson as A
import Control.Monad
import Control.Monad.State
import Control.Concurrent

import TaskPals
import SampleWorld hiding (main)


main :: IO ()
main = runServer "0.0.0.0" 3000 $ \request -> do
    -- liftIO $ putStrLn $ show request
    acceptRequest request
    loop world
    
delta = 10000

loop world = do
    sendTextData $ A.encode world :: WebSockets Hybi10 ()
    liftIO $ threadDelay delta
    let world' = execState (tick (fromIntegral delta) []) world
    loop world'
