{-# LANGUAGE OverloadedStrings #-}

module Within.DBMemorial.Views.Thread where

import           Control.Monad               (forM_)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import           Text.Blaze.Html
import           Text.Blaze.Html5            as H hiding (map)
import           Text.Blaze.Html5.Attributes as A
import qualified Within.DBMemorial.Post      as Post
import qualified Within.DBMemorial.User      as User

render :: Int -> [Post.Post] -> (Map Text User.User) -> Html
render pageNumber posts users = do
    H.h1 ! A.class_ "center text-orange" $ H.toHtml ("Page " ++ (show pageNumber))

    H.div ! A.class_ "row" $ do
      forM_ posts $ \post -> do
        let user = case Map.lookup (Post.author post) users of
                Just u -> u
                Nothing -> error "Can't find user? Impossible state."

        Post.render post user
