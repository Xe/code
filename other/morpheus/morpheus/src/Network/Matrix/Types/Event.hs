{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Network.Matrix.Types.Event where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

data Type = PowerLevels
          | JoinRules
          | AddStateLevel
          | SendEventLevel
          | OpsLevels
          | Member
          | Invite
          | Message
          | Other Text
          deriving (Eq, Show)

data Event = Event
           { type'   :: Text
           , eventID :: Text
           , roomID  :: Text
           , sender  :: Text
           , content :: Map.HashMap Text Text
           } deriving(Show)

instance FromJSON Event where
        parseJSON (Object v) = Event
                               <$> v .: "type"
                               <*> v .: "event_id"
                               <*> v .: "room_id"
                               <*> v .: "sender"
                               <*> v .: "content"
