module Within.DBMemorial.Utils where

import Data.Text (Text)
import qualified Data.Text                            as Text
import           Data.Text.Lazy                       (toStrict)
import           Text.Blaze.Html (Html)
import           Text.Blaze.Renderer.Text
import qualified Within.DBMemorial.Views.Materialize  as Materialize

-- https://hackage.haskell.org/package/base-4.8.0.0/docs/src/Data-Function.html#%26
(&) :: a -> (a -> b) -> b
x & f = f x

wrapMarkup :: Text -> Html -> Text
wrapMarkup title body =
    toStrict $ renderMarkup $ Materialize.render title body

wrapMarkupStringTitle :: String -> Html -> Text
wrapMarkupStringTitle title body =
        wrapMarkup (Text.pack title) body
