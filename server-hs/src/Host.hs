module Host (runHost) where

import Message

import Network.WebSockets
import Control.Monad (forever)
import Data.Text (Text)

-- ゲームのメインループ
gameLoop :: ServerApp
gameLoop pendingConn = do
    conn <- acceptRequest pendingConn
    forever $ do
        msg <- receiveData conn :: IO Text
        print msg
        sendTextData conn msg
        return ()

runHost :: IO ()
runHost = runServer "127.0.0.1" 8080 gameLoop