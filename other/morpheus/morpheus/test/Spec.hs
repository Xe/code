{-# LANGUAGE OverloadedStrings #-}

import Network.Matrix
import Test.Hspec (Spec, hspec, describe, it, shouldSatisfy, shouldNotSatisfy, shouldBe)
import System.Environment (getEnv)
import qualified Data.Text as Text
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Network.Matrix.Types.Event

spec :: Spec
spec =
        describe "Matrix client" $ do
            it "should let you log into an account" $ do
                username'   <- getEnv "MATRIX_USER"
                password'   <- getEnv "MATRIX_PASSWORD"
                homeserver' <- getEnv "MATRIX_HOMESERVER"
                c <- login (Text.pack username') (Text.pack password') (Text.pack homeserver')

                username c `shouldBe` Text.pack username'
            it "has function foo" $
                foo `shouldBe` "bar"

anEvent :: String
anEvent = "{ \"content\": { \"body\": \"Hello world!\", \"msgtype\": \"m.text\" }, \"room_id\": \"!wfgy43Sg4a:matrix.org\", \"sender\": \"@bob:matrix.org\", \"event_id\": \"$asfDuShaf7Gafaw:matrix.org\", \"type\": \"m.room.message\" }"

eventSpec :: Spec
eventSpec =
        describe "An Event" $
            it "should be parseable from json" $ do
                let myEvent = eitherDecode (C.pack anEvent) :: Either String Event

                case myEvent of
                    Left e  -> error e
                    Right e -> print e

main :: IO ()
main = hspec spec >> hspec eventSpec
