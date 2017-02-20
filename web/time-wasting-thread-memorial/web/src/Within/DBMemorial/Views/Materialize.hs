{-# LANGUAGE OverloadedStrings #-}

module Within.DBMemorial.Views.Materialize where

import qualified Clay                        (renderWith, compact)
import           Data.Monoid
import           Data.Text
import           Text.Blaze.Html
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A
import qualified Within.DBMemorial.CSS.Base  as CSSBase

render :: Text -> Html -> Html
render pageTitle inner =
    docTypeHtml ! lang "en" $ do
      H.head $ do
        H.title $ toHtml pageTitle

        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        link ! rel "stylesheet" ! href "/materialize/css/materialize.min.css"

        H.style $ H.toHtml $ Clay.renderWith Clay.compact [] $ CSSBase.render

        script ! type_ "text/javascript" ! src "https://code.jquery.com/jquery-2.1.1.min.js" $ mempty
        script ! src "/materialize/js/materialize.min.js" $ mempty
      H.body $ do
        H.div ! class_ "container" $ do
          inner
