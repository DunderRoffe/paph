{-# LANGUAGE OverloadedStrings #-}
module Handshake where

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
  
