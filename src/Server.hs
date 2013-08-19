{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Server where

import Network.WebSockets
import qualified Data.Aeson as A
import Data.Aeson.TH
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
import qualified Data.Char as Char

import TaskPals
import SampleWorld

data ServerCommand = GameCommand Command | Join Player
deriveJSON (dropWhile (not . Char.isUpper)) ''ServerCommand

deriveJSON (dropWhile (not . Char.isUpper)) ''Skill
deriveJSON (dropWhile (not . Char.isUpper)) ''SkillType
deriveJSON (dropWhile (not . Char.isUpper)) ''WorkType
deriveJSON (dropWhile (not . Char.isUpper)) ''Command
deriveJSON (dropWhile (not . Char.isUpper)) ''Location
deriveJSON (dropWhile (not . Char.isUpper)) ''Destination
deriveJSON (dropWhile (not . Char.isUpper)) ''Goal
deriveJSON (dropWhile (not . Char.isUpper)) ''Task
deriveJSON (dropWhile (not . Char.isUpper)) ''PhysicsComponent
deriveJSON (dropWhile (not . Char.isUpper)) ''MetaComponent
deriveJSON (dropWhile (not . Char.isUpper)) ''TaskEvent
deriveJSON (dropWhile (not . Char.isUpper)) ''Shape
deriveJSON (dropWhile (not . Char.isUpper)) ''GoalPref
deriveJSON (dropWhile (not . Char.isUpper)) ''Target
deriveJSON (dropWhile (not . Char.isUpper)) ''Work
deriveJSON (dropWhile (not . Char.isUpper)) ''WorkComponent
deriveJSON (dropWhile (not . Char.isUpper)) ''World

data GameConfig = GameConfig
type View = B.ByteString
data Admin = SetState World

data GameServer = GameServer
    { _state :: T.TVar World
    , _commands :: T.TVar [Command]
    , _configuration :: GameConfig
    --, _players :: T.TVar [(Player, Input View)]
    , _subscribers :: T.TVar [(Player, Input View)]
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
        subscribers <- readTVar (_subscribers gameServer)
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
            atomically $ modifyTVar (_subscribers gameServer) (subscriber :)
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
    case A.decode mCommand of
        Just command -> do sent <- liftIO . atomically $ Control.Proxy.Concurrent.send commandIn command
                           if sent then spamServer commandIn else spamServer commandIn
        Nothing -> spamServer commandIn
