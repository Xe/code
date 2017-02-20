{-# LANGUAGE OverloadedStrings #-}

module Within.DBMemorial.Views.Error where

import           Text.Blaze.Html
import           Text.Blaze.Html5            as H hiding (map)
import           Text.Blaze.Html5.Attributes as A

render :: String -> Html
render why = do
    H.div ! A.class_ "card red darken-3" $ do
        H.div ! A.class_ "card-content white-text center user-card" $ do
            H.h1 ! A.class_ "center text-orange" $ "404"
            H.p $                 "Sorry! We couldn't find any data. If this isn't meant to be seen, please let the maintainers know:"
            H.pre $ H.toHtml why
