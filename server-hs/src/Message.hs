{-# LANGUAGE DeriveGeneric #-}
module Message where

import Network.WebSockets (WebSocketsData(..), DataMessage(..))
import Data.Aeson
import GHC.Generics
import Data.Text


-- client と server でやりとりするメッセージ
newtype Msg = Msg {
    messsage :: Text
} deriving (Generic, Show)

instance ToJSON Msg
instance FromJSON Msg

instance WebSocketsData Msg where
  fromLazyByteString bs
    | Just msg <- decode bs = msg
    | otherwise = Msg ""
  toLazyByteString = encode
  fromDataMessage (Text bl _) = fromLazyByteString bl
  fromDataMessage (Binary bl) = fromLazyByteString bl