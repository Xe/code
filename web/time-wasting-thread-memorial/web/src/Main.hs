{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.IO.Class
import qualified Data.Map                             as Map
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Database.SQLite.Simple               as Db
import qualified Network.HTTP.Types.Status            as Status
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.Wai.Middleware.Static        as Static
import           Web.Spock                            ((<//>))
import qualified Web.Spock                            as S
import qualified Within.DBMemorial.Post               as Post
import qualified Within.DBMemorial.User               as User
import           Within.DBMemorial.Utils
import qualified Within.DBMemorial.Views.Error        as ErrorView
import qualified Within.DBMemorial.Views.Thread       as Thread

main :: IO ()
main = do
    conn <- Db.open "../db/posts.db"

    -- Simple tests to prove we're reading from SQLite
    r <- Db.query_ conn "SELECT * FROM Posts WHERE page=1" :: IO [Post.Post]
    let h = head r
    users <- Db.query_ conn "SELECT * FROM Users WHERE oid='51315c97a4c72da155001b9a' LIMIT 1" :: IO [User.User]
    let user = head users

    allUsers <- Db.query_ conn "SELECT * FROM Users" :: IO [User.User]
    let userMap = Map.fromList [(User.oID u, u) | u <- allUsers]

    -- Set up the URL router
    S.runSpock 5000 $ S.spockT id $ do
        S.middleware $ Static.staticPolicy $ Static.addBase "public"
        S.middleware $ RequestLogger.logStdout

        -- 404 handler
        S.hookAny S.GET $ \(paths :: [Text]) ->
            S.setStatus Status.status404 >>

            let slash = Text.pack "/"
                path = Text.append slash $ Text.intercalate slash paths
                message = "No such page at " ++ (Text.unpack path)
                page = ErrorView.render message
                title = "Page not found"

            in S.html $ wrapMarkupStringTitle title page

        -- Test routes
        S.get S.root $
            S.html $ wrapMarkup "The OP" (Post.render h user)

        S.get "user" $
            S.html $ wrapMarkup "The first poster" (User.render user)

        -- Real routes
        S.get ("page" <//> S.var) $ \(id_ :: Int) -> do
            posts <- liftIO $
                Db.query conn "SELECT * FROM Posts WHERE page=?" (Db.Only (id_ :: Int))
                    :: S.ActionT IO [Post.Post]

            case posts of
                [] ->
                    S.setStatus Status.status404 >>

                    let message = ("no data for page number " ++ (show id_))
                        page = ErrorView.render message
                        title = ("Can't find page " ++ (show id_))

                    in S.html $ wrapMarkupStringTitle title page

                _ ->
                    let title = "Thread page " ++ (show id_)
                        page = Thread.render id_ posts userMap

                    in S.html $ wrapMarkupStringTitle title page


