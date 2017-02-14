{-# LANGUAGE OverloadedStrings, DeriveDataTypeable  #-}

module Main where

import Control.Monad (msum, liftM)
import Data.Char (toLower)
import Data.Monoid (mempty)
import Happstack.Server
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import DatabaseAccess as DA
import Control.Monad.IO.Class (liftIO)
import Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as L
import PunctuationParrotTypes.FrontEndPost


--gamePage :: Response
gamePage = toResponse $
    H.html $ do
        H.head $ do
            H.title "Punctuation Parrot!"
            H.script ! A.src "js/canvas.js" $ mempty
            H.script ! A.src "js/parser.js" $ mempty
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "css/app.css"
        H.body ! A.onload "gameInit()" $ do
            H.div ! A.id "modal-overlay" $ do
                H.div ! A.id "modal" $ do
                    H.h3 ! A.id "modal-title" $ "Your result:"
                    H.p ! A.id "modal-text" $ mempty
                    H.div ! A.id "close" $ "Close"

            H.div ! A.id "container" $ do
                H.img ! A.src "images/parrot.png" ! A.width "73" ! A.height "64"
                H.span ! A.id "title" $ "Punctuation Parrot!"
                H.canvas ! A.id "game" ! A.width "1200" ! A.height "600" $ "Please upgrade your browser."
            H.script ! A.src "js/init.js" $ mempty

--getSentence :: Int -> IO Data.ByteString.Lazy.Internal.ByteString
getSentence userId = do
    sent <- DA.fetchUserSentence userId
    return $ AE.encode sent

getBody :: ServerPart L.ByteString
getBody = do
    req <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
        Just rqbody -> return . unBody $ rqbody
        Nothing     -> return "No body found"

--updateAttempt :: ServerPart Response
updateAttempt = do
    body <- getBody
    let post = AE.decode body :: Maybe FrontEndPost
    let result = DA.sentenceUpdate post
    x <- liftIO result
    return $ toResponse $ show x


main :: IO ()
main = simpleHTTP nullConf $ msum
  [ dir "js" $ serveDirectory DisableBrowsing [] "../js",
    dir "css" $ serveDirectory DisableBrowsing [] "../assets/css",
    dir "images" $ serveDirectory DisableBrowsing [] "../assets/images",
    dir "getSentence" $ liftIO (Main.getSentence 1) >>= \res -> ok (toResponse res),
    dir "play" $ ok $ gamePage,
    dir "updateStats" $ updateAttempt >>= \res -> ok res
  ]



{-


ok, so this formulation works

updateAttempt :: ServerPartT IO Response
updateAttempt = do
    body <- getBody
    let post = AE.decode body :: Maybe FrontEndPost
    let result = DA.sentenceUpdate post
    x <- liftIO result
    return $ toResponse x


...
dir "updateStats" $ updateAttempt >>= \res -> ok res

but why not

updateAttempt :: ServerPartT IO (IO String)
updateAttempt = do
    body <- getBody
    let post = AE.decode body :: Maybe FrontEndPost
    return $ DA.sentenceUpdate post


...

dir "updateStats" $ liftIO updateAttempt >>= \res -> ok $ toResponse res


-}