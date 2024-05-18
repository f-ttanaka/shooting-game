module Host (runHost) where

import Message

import Network.WebSockets
import Control.Monad (forever)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Data.List (nub)
import Data.ByteString.Lazy.Char8 (pack)

type Client = (Text, Connection)
type ServerState = [Connection]

addClient :: Connection -> ServerState -> ServerState
addClient conn clients = conn : clients

broadcast :: Msg -> ServerState -> IO ()
broadcast message clients = 
    forM_ clients $ \conn -> sendTextData conn message

-- ゲームのメインループ
gameLoop :: MVar ServerState -> ServerApp
gameLoop state pendingConn = do
    conn <- acceptRequest pendingConn
    putStrLn "connected"
    modifyMVar_ state $ \conns -> return (addClient conn conns)
    forever $ do
        msg <- receiveData conn :: IO Msg
        conns' <- readMVar state
        print msg
        broadcast msg conns'

runHost :: IO ()
runHost = do
    state <- newMVar []
    runServer "127.0.0.1" 8080 $ gameLoop state