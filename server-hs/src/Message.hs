{-# LANGUAGE DeriveGeneric #-}
module Message where

import Network.WebSockets (WebSocketsData(..), DataMessage(..))
import Data.Aeson
import GHC.Generics
import qualified Data.Binary as Binary
import Data.Text
import qualified Data.ByteString.Lazy as LBS


-- client と server でやりとりするメッセージ
data Msg = Msg {
    cname :: Text,
    cmessage :: Text
  } deriving (Generic, Show)

instance ToJSON Msg
instance FromJSON Msg

instance WebSocketsData Msg where
  fromLazyByteString bs = case decode bs of
    Just msg -> msg
    _ -> Msg "no name" ""
  toLazyByteString = encode
  fromDataMessage (Text bl _) = fromLazyByteString bl
  fromDataMessage (Binary bl) = fromLazyByteString bl