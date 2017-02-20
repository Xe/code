{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Data.Monoid
import System.Environment
import Web.Spock.Safe

main :: IO ()
main = do
    target' <- getEnv "FORWARD_URL"
    let target = pack target'
    runSpock 8080 $ spockT id $ do
        hookAny GET  $ \_ -> redirect target
        hookAny POST $ \_ -> redirect target
