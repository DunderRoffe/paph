{-# LANGUAGE OverloadedStrings #-}

import ServerState
import ClientHandling

import Data.Aeson
import Control.Exception (finally)
import Control.Monad (unless)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
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
