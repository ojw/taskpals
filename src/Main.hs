module Main where

import TaskPals
import Server
import SampleWorld
import Control.Concurrent

main :: IO ()
main = do
    putStrLn "Initializing game server..."
    gameServer <- newGameServer world
    forkIO $ runGameServer gameServer
    putStrLn "Initializing websocket server..."
    startWebSocketServer testConfig (_subscribeIn gameServer) (_commandIn gameServer)

