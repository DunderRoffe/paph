module ServerState where

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
