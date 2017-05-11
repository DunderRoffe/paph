{-# LANGUAGE OverloadedStrings #-}

import Message
import Data
import ServerState

import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Aeson
import Control.Exception (finally)
import Control.Monad (unless)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state

-- | Starts the application with a server state
application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    client <- handshake conn state
    flip finally (disconnect client state) $ do
       connect client state
       clientHandler client state

-- | Waits until a specific message is recieved on the given connection.
waitForMessage :: Message -> WS.Connection -> IO ()
waitForMessage msgMatch conn = do
    putStrLn $ "Waiting for a " ++ show msgMatch
    jsonMsg <- WS.receiveData conn

    let (Just recievedMsg) = decode jsonMsg
    unless (msgMatch == recievedMsg) $ do
        putStrLn $ "Ignoring unexpected msg: " ++ show recievedMsg
        waitForMessage msgMatch conn

-- | Connect a client to the server
connect :: Client -> MVar ServerState -> IO ()
connect client@(vid, conn) state = do
    WS.sendTextData conn (encode (Connect vid))
    print $ "A user has claimed slot " ++ show vid
    liftIO $ modifyMVar_ state $ \s ->
        return $ addClient client s

-- | Disconnect a client from the server
disconnect :: Client -> MVar ServerState -> IO ()
disconnect client state = do
    modifyMVar state $ \s ->  let s' = removeClient client s in return (s', s')
    putStrLn $ show (fst client) ++ " has disconnected"

-- | Handles requests for a client.
-- Loops until a disconnect message is recieved.
clientHandler :: Client -> MVar ServerState -> IO ()
clientHandler client@(_, conn) state = do
    jsonMsg <- WS.receiveData conn
    case decode jsonMsg of
        Just Disconnect -> return ()
        Just msg -> do
            liftIO $ readMVar state >>= broadcast msg
            clientHandler client state
        Nothing -> clientHandler client state


-- | Perform a handshake using a specific connection
handshake :: WS.Connection -> MVar ServerState -> IO Client
handshake conn state = do
    -- Wait for a Ready message and then respond with available spots
    waitForMessage Ready conn
    s <- liftIO $ readMVar state
    WS.sendTextData conn (encode (Available (available s)))

    -- Next message needs to be a Connect
    jsonMsg <- WS.receiveData conn
    case decode jsonMsg of
        Just (Connect vid) -> return (vid, conn)
                              -- TODO Check if client already connected
        _                  -> error "Illegal handshake, Did not receive a Connect"
