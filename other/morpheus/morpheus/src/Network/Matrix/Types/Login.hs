{-# LANGUAGE OverloadedStrings #-}

module Network.Matrix.Types.Login where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text

data Login = Login
           { type'    :: Text
           , username :: Text
           , password :: Text
           } deriving (Show)

instance ToJSON Login where
    toJSON (Login type' username password) =
        object [ "type"     .= type'
               , "user"     .= username
               , "password" .= password
               ]
