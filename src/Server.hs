{-# LANGUAGE OverloadedStrings #-}

module Server where

import Network.WebSockets
import qualified Data.Aeson as A
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Lazy as B
import Control.Proxy.Concurrent
import qualified Control.Proxy.Concurrent as Concurrent
import Control.Lens
import Control.Concurrent.STM.TVar as T
import Control.Monad.State
import Control.Applicative

import TaskPals
import SampleWorld hiding (main)

data GameConfig = GameConfig
type View = B.ByteString
data Admin = SetState World

data GameServer = GameServer
    { _state :: T.TVar World
    , _commands :: T.TVar [Command]
    , _configuration :: GameConfig
    , _players :: T.TVar [(Player, Input View)]
    , _commandIn :: Input Command
    , _commandOut :: Output Command
    , _adminIn :: Input Admin
    , _adminOut :: Output Admin
    , _subscribeIn :: Input (Player, Input View)
    , _subscribeOut :: Output (Player, Input View)
    }

newGameServer :: World -> IO GameServer
newGameServer world = do
    (state, commands, players) <- atomically $ (,,) <$> newTVar world <*> newTVar [] <*> newTVar []
    (commandIn, commandOut) <- spawn Unbounded
    (adminIn, adminOut) <- spawn Unbounded
    (subscribeIn, subscribeOut) <- spawn Unbounded
    return $ GameServer state commands GameConfig players commandIn commandOut adminIn adminOut subscribeIn subscribeOut

data SocketServerConfig = SocketServerConfig
    { _address :: String
    , _port :: Int
    }

main :: IO ()
main = do
    putStrLn "Initializing game server..."
    gameServer <- newGameServer world
    forkIO $ runGameServer gameServer
    putStrLn "Initializing websocket server..."
    startWebSocketServer testConfig (_subscribeIn gameServer) (_commandIn gameServer)

delta = 10000

testConfig :: SocketServerConfig
testConfig = SocketServerConfig "0.0.0.0" 3000

runGameServer :: GameServer -> IO ()
runGameServer gameServer = do
    _ <- forkIO $ commandWorker gameServer
    _ <- forkIO $ subscribeWorker gameServer
    tickWorker gameServer

tickWorker :: GameServer -> IO ()
tickWorker gameServer = do 
    (state', subscribers) <- atomically $ do
        commands <- readTVar (_commands gameServer)
        subscribers <- readTVar (_players gameServer)
        writeTVar (_commands gameServer) []
        modifyTVar (_state gameServer) (execState $ tick 10000 commands)
        state' <- readTVar (_state gameServer)
        return (state', subscribers)
    mapM_ (\(_,viewIn) -> atomically $ Concurrent.send viewIn $ A.encode state')  subscribers
    threadDelay 10000
    tickWorker gameServer
    
subscribeWorker :: GameServer -> IO ()
subscribeWorker gameServer = do
    mSubscribe <- atomically $ recv (_subscribeOut gameServer)
    case mSubscribe of
        Just subscriber -> do
            putStrLn $ "Registering " ++ show (fst subscriber) ++ " with game server... "
            atomically $ modifyTVar (_players gameServer) (subscriber :)
            subscribeWorker gameServer
        Nothing -> subscribeWorker gameServer

commandWorker :: GameServer -> IO ()
commandWorker gameServer = do
    mCommand <- atomically $ recv (_commandOut gameServer)
    case mCommand of
        Just command -> atomically $ modifyTVar (_commands gameServer) (command :)
        Nothing -> commandWorker gameServer -- should really die
    putStrLn $ show mCommand
    commandWorker gameServer
    
startWebSocketServer :: SocketServerConfig -> Input (Player, Input View) -> Input Command -> IO ()
startWebSocketServer (SocketServerConfig addr port) subscribeIn commandIn = runServer addr port $ \request -> do 
    acceptRequest request
    sink <- getSink
    (viewIn, viewOut) <- liftIO $ spawn Unbounded
    _ <- liftIO . atomically $ Control.Proxy.Concurrent.send subscribeIn ("James", viewIn)
    liftIO . putStrLn $ "Client connected."
    liftIO . forkIO $ spamClient viewOut sink
    spamServer (commandIn)

spamClient :: Output View -> Sink Hybi00 -> IO ()
spamClient viewOut sink = do
    state <- atomically $ recv viewOut
    maybe (spamClient viewOut sink) (\view -> sendSink sink (textData view) >> spamClient viewOut sink) state

spamServer :: Input Command -> WebSockets Hybi00 ()
spamServer commandIn = do
    mCommand <- receiveData
    liftIO . Char8.putStrLn $ mCommand
    case A.decode mCommand of
        Just command -> do sent <- liftIO . atomically $ Control.Proxy.Concurrent.send commandIn command
                           if sent then spamServer commandIn else spamServer commandIn
        Nothing -> spamServer commandIn
