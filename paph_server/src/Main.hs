{-# LANGUAGE OverloadedStrings #-}

import Handshake
import Message
import Data

import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Aeson
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

type Client = (Int, WS.Connection)
data ServerState = ServerState {
  clients :: [Client],
  available :: [Int]
  }

newServerState :: ServerState
newServerState = ServerState [] [1,2,3]

numClients :: ServerState -> Int
numClients state = length $ clients state

clientExists :: Client -> ServerState -> Bool
clientExists client state = any ((== fst client) . fst) (clients state)

addClient :: Client -> ServerState -> ServerState
addClient client state = state { clients = client : clients state }

removeClient :: Client -> ServerState -> ServerState
removeClient client state = state { clients = filter ((/= fst client) . fst) (clients state) }

broadcast :: Message -> ServerState -> IO ()
broadcast message (ServerState clients _) = do
    let textMessage = T.pack $ show $ encode message
    T.putStrLn textMessage
    forM_ clients $ \(_, conn) -> WS.sendTextData conn textMessage

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    jsonMsg <- WS.receiveData conn
    s <- liftIO $ readMVar state
    case decode jsonMsg of
        Just (PLZ) -> do
          print "Got plz"
          WS.sendTextData conn (encode (Available (available s)))
          handshake conn state
        _ -> putStrLn $ "Got '" ++ show jsonMsg ++ "' which can not be parsed to either PLZ or Connect"

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
