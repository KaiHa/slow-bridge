{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>), optional)
import Control.Monad (msum, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit)
import Data.List (find, isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (unpack)
import Happstack.Server hiding (body)
import System.Process (readProcess)
import Text.Blaze.Html5 (Html, (!), form, input, toHtml, toValue, label)
import Text.Blaze.Html5.Attributes (action, enctype, name, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = do
    putStrLn "Open http://localhost:8000/netem in a browser"
    simpleHTTP nullConf $ do
        decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
        handlers


newtype Nic = Nic {unNic :: String}


handlers :: ServerPart Response
handlers = do
    nics <- liftIO getNICs
    msum [ dir "netem" $ netem nics
         , seeOther ("/netem" :: String) $ toResponse ()
         ]
  where
    getNICs :: IO [Nic]
    getNICs = do
        ls <- ip ["link"]
        let nics = [strip x | x <- lines ls, isDigit $ head x]
        return [Nic x | x <- nics, x /= "lo" && take 3 x /= "vir"]
        where strip = takeWhile (/= ':') . drop 2 . dropWhile (/= ':')


netem :: [Nic] -> ServerPart Response
netem ns = msum [viewNetem ns, updateNetem]


viewNetem :: [Nic] -> ServerPart Response
viewNetem ns = do
    method GET
    nic1   <- optional $ lookCookieValue "nic1"
    nic2   <- optional $ lookCookieValue "nic2"
    rate1  <- optional $ lookCookieValue "rate1"
    rate2  <- optional $ lookCookieValue "rate2"
    delay1 <- optional $ lookCookieValue "delay1"
    delay2 <- optional $ lookCookieValue "delay2"
    lsBr   <- liftIO $ brctl ["show"]
    lsTc   <- liftIO $ tc ["qdisc"]
    ok $ toResponse $ H.html $ do
        H.head $ H.title "Network Emulator"
        H.body $ do
            H.h1 "Network Emulator"
            form ! action "/netem" ! enctype "multipart/form-data" ! A.method "POST" $ do
                H.p $ do label ! A.for "nics" $ "Select the network interface cards to bridge: "
                         H.select ! name "new_nic1" $ nics nic1
                         label " <-> "
                         H.select ! name "new_nic2" $ nics nic2
                H.p $ do label ! A.for "rate2" $ fromTo nic1 nic2
                         H.select ! name "new_rate2"  ! ralign $ rates rate2
                         label " Delay: "
                         H.select ! name "new_delay2" ! ralign $ delays delay2
                H.p $ do label ! A.for "rate1" $ fromTo nic2 nic1
                         H.select ! name "new_rate1"  ! ralign $ rates rate1
                         label " Delay: "
                         H.select ! name "new_delay1" ! ralign $ delays delay1
                H.p $ input ! type_ "submit" ! value "Apply"
            textarea "brctl show:" lsBr "5"
            textarea "tc qdisc:"   lsTc "12"
  where
    rates def  = do toOpt def   "32kbit"
                    toOpt def   "64kbit"
                    toOpt def  "128kbit"
                    toOpt def  "256kbit"
                    toOpt def  "512kbit"
                    toOpt def "1024kbit"
    delays def = do toOpt def  "10ms"
                    toOpt def  "40ms"
                    toOpt def "160ms"
                    toOpt def "320ms"
                    toOpt def "640ms"
    nics def = mapM_ (toOpt def . unNic) ns
    ralign = A.style "text-align:right"
    textarea caption text n =
        H.section $ do
            H.h1 caption
            H.p $ H.textarea (H.toHtml text)
                   ! A.rows n
                   ! A.cols "100"
                   ! A.readonly "readonly"
                   ! A.style "font-family:monospace"
    toOpt :: Maybe String -> String -> Html
    toOpt def v = (if Just v == def then (! A.selected "") else id)
                  $ H.option (toHtml v) ! value (toValue v)
    fromTo a b = do _ <- "From "
                    H.b $ toHtml $ fromMaybe "A" a
                    _ <- " to "
                    H.b $ toHtml $ fromMaybe "B" b
                    ": "


updateNetem :: ServerPart Response
updateNetem = do
    method POST
    nic1   <- unpack <$> lookText "new_nic1"
    nic2   <- unpack <$> lookText "new_nic2"
    rate1  <- unpack <$> lookText "new_rate1"
    rate2  <- unpack <$> lookText "new_rate2"
    delay1 <- unpack <$> lookText "new_delay1"
    delay2 <- unpack <$> lookText "new_delay2"
    liftIO $ updateBridge nic1 nic2
    liftIO $ tc' nic1 delay1 rate1
    liftIO $ tc' nic2 delay2 rate2
    addCookies [ (Session, mkCookie "nic1"   nic1)
               , (Session, mkCookie "nic2"   nic2)
               , (Session, mkCookie "rate1"  rate1)
               , (Session, mkCookie "rate2"  rate2)
               , (Session, mkCookie "delay1" delay1)
               , (Session, mkCookie "delay2" delay2)
               ]
    seeOther ("/netem" :: String) (toResponse ())
  where
    ipLink a = void $ ip ("link":a)
    tc' n d r =
        void $ tc ["qdisc", "replace", "dev", n, "root", "netem"
                  , "delay", d, "rate", r]
    updateBridge nic1 nic2 = do
        b <- find ("brSlow" `isPrefixOf`) <$> lines <$> brctl ["show"]
        when (isDeleteNeeded b) deleteBridge
        when (isUpdateNeeded b) updateBridge'
      where
        isUpdateNeeded Nothing  = True
        isUpdateNeeded (Just b) | not (nic1 `isInfixOf` b) = True
                                | not (nic2 `isInfixOf` b) = True
                                | otherwise                = False
        isDeleteNeeded Nothing  = False
        isDeleteNeeded b        = isUpdateNeeded b
        deleteBridge  = do ipLink ["set", "down", "dev", "brSlow"]
                           void $ brctl ["delbr", "brSlow"]
        updateBridge' = do void $ brctl ["addbr", "brSlow"]
                           void $ brctl ["setfd", "brSlow", "0"]
                           void $ brctl ["addif", "brSlow", nic1]
                           void $ brctl ["addif", "brSlow", nic2]
                           ipLink ["set", "up", "dev", nic1]
                           ipLink ["set", "up", "dev", nic2]
                           ipLink ["set", "up", "dev", "brSlow"]


brctl :: [String] -> IO String
brctl a = readProcess "brctl" a ""

ip :: [String] -> IO String
ip a = readProcess "ip" a ""

tc :: [String] -> IO String
tc a = readProcess "tc" a ""
