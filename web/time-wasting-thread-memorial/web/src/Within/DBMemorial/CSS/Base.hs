{-# LANGUAGE OverloadedStrings #-}

module Within.DBMemorial.CSS.Base where

import           Clay    hiding (render)
import           Clay    ()
import           Prelude hiding (div)

render :: Css
render = do
        ".card" ?
            padding nil nil nil nil

        div ?
            overflow hidden
