{-# LANGUAGE OverloadedStrings #-}
module Within.DBMemorial.Post where

import           Control.Applicative
import           Data.Monoid                    ()
import           Data.Text
import           Database.SQLite.Simple.FromRow
import           Text.Blaze.Html5               ((!))
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import           Text.Pandoc
import qualified Within.DBMemorial.User         as User

data Post = Post
          { id       :: Int
          , oID      :: Text
          , body     :: Text
          , markdown :: Text
          , author   :: Text
          , page     :: Int
          } deriving (Show, Eq)

instance FromRow Post where
    fromRow = Post <$> field <*> field <*> field <*> field <*> field <*> field

render :: Post -> User.User -> H.Html
render post user =
    let state = case readTextile def (unpack $ body post) of
          Right v -> v
          Left err -> error (show err)

    in do
        H.div ! A.class_ "card-panel blue-grey darken-1" $ do
          H.div ! A.class_ "card-content white-text" $ do
            H.div ! A.class_ "row" $ do
              H.div ! A.class_ "col s12 m3" $ User.render user
              H.div ! A.class_ "col s12 m9" $ writeHtml def state
