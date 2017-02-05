{-# LANGUAGE OverloadedStrings #-}

import Message
import Data
import ServerState

import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Aeson
import Control.Exception (finally)
import Control.Monad (forM_, forever, unless)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    waitForMessage Ready conn
    s <- liftIO $ readMVar state
    WS.sendTextData conn (encode (Available (available s)))
    handshake conn state

waitForMessage msgMatch conn = do
    putStrLn $ "Waiting for a " ++ show msgMatch
    jsonMsg <- WS.receiveData conn

    let (Just recievedMsg) = decode jsonMsg
    unless (msgMatch == recievedMsg) $ do
        putStrLn $ "Ignoring unexpected msg: " ++ show recievedMsg
        waitForMessage msgMatch conn

disconnect :: Client -> MVar ServerState -> IO ()
disconnect client state = do
    modifyMVar state $ \s ->  let s' = removeClient client s in return (s', s')
    putStrLn (show (fst client) ++ " has disconnected" )

clientHandler :: WS.Connection -> MVar ServerState -> Client -> IO ()
clientHandler conn state client = do
    jsonMsg <- WS.receiveData conn
    case decode jsonMsg of
        Just Disconnect -> return ()
        Just msg -> do
            liftIO $ readMVar state >>= broadcast msg
            clientHandler conn state client
        Nothing -> clientHandler conn state client

handshake :: WS.Connection -> MVar ServerState -> IO ()
handshake conn state = do
  print "Waiting for handshake"
  jsonMsg <- WS.receiveData conn
  case decode jsonMsg of
    Just (Connect vid) -> let client = (vid, conn) in
           flip finally (disconnect client state) $ do
           WS.sendTextData conn (encode (Connect vid))
           print $ "A user has claimed slot " ++ show vid
           liftIO $ modifyMVar_ state $ \s -> do
               let s' = addClient client s
--               WS.sendTextData conn $ T.pack "Welcome to the server: "
               return s'
           clientHandler conn state (vid, conn)
    _ -> putStrLn $ "Illegal handshake, Did not receive a Connect"
