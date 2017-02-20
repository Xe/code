{-# LANGUAGE OverloadedStrings #-}
module Within.DBMemorial.User where

import           Control.Applicative
import           Data.Monoid                    ()
import           Data.Text
import           Database.SQLite.Simple.FromRow
import           Text.Blaze.Html5               ((!))
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

data User = User
          { id     :: Int
          , oID    :: Text
          , name   :: Text
          , avatar :: Text
          } deriving (Show, Eq)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field

render :: User -> H.Html
render user = do
    H.div ! A.class_ "card blue-grey darken-3" $ do
        H.div ! A.class_ "card-content white-text center user-card" $ do
            H.h5 $ H.toHtml $ unpack (name user)
            H.br
            H.a ! A.href (H.toValue ("https://derpibooru.org/profiles/" ++ unpack (oID user))) $ do
                H.img ! A.src (H.toValue $ unpack $ avatar user) ! A.width "125"
        H.div ! A.class_ "card-action blue-text" $ do
            H.a ! A.href (H.toValue ("/users/" ++ (unpack (oID user)))) $ "All their posts"
            H.br
            H.a ! A.href (H.toValue ("https://derpibooru.org/profiles/" ++ (unpack (oID user)))) $ "Derpibooru profile"
