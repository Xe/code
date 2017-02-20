{-# LANGUAGE OverloadedStrings #-}

module Network.Matrix where

import Control.Lens
import Data.Aeson
import qualified Data.Aeson (Value (String))
import Data.Aeson.Lens (_String, key)
import Data.ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Wreq

import qualified Network.Matrix.Types.Login as Login
import qualified Network.Matrix.Types.Event as Event

foo :: String
foo = "bar"

data ClientHandle = ClientHandle
                  { username   :: Text
                  , token      :: Text
                  , homeserver :: Text
                  } deriving (Show)

urlGen :: ClientHandle -> String -> String
urlGen c =
        (++) homeserver'
        where
            homeserver' = Text.unpack $ homeserver c

login :: Text -> Text -> Text -> IO ClientHandle
login username' password homeserver' = do
        let url = Text.unpack homeserver' ++ "/_matrix/client/api/v1/login" :: String

        r <- post url (toJSON Login.Login
                        { Login.type'    = Text.pack "m.login.password"
                        , Login.username = username'
                        , Login.password = password
                        })

        let accessToken' = r ^. responseBody . key (Text.pack "access_token") . _String

        pure ClientHandle
            { username   = username'
            , token      = accessToken'
            , homeserver = homeserver'
            }
