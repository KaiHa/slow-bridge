{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), optional)
import Control.Monad (msum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Server hiding (body)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = simpleHTTP nullConf
       $  do decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
             handlers


handlers :: ServerPart Response
handlers = msum
  [ dir "netem"   $ netem
  , netem
  ]


template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body


netem :: ServerPart Response
netem = msum [ viewNetem, updateNetem ]
    where
      viewNetem :: ServerPart Response
      viewNetem =
          do method GET
             rate1  <- lookCookieValue' "512kbit" "rate1"
             rate2  <- lookCookieValue' "512kbit" "rate2"
             delay1 <- lookCookieValue' "10ms"    "delay1"
             delay2 <- lookCookieValue' "10ms"    "delay2"
             ok $ template "Network Emulator" $ do
                    H.h1 "Network Emulator"
                    H.p "Would set network emulation to:"
                    H.p $ toHtml $ rate1 ++ " " ++ delay1 ++ " / " ++ rate2 ++ " " ++ delay2
                    form ! action "/netem" ! enctype "multipart/form-data" ! A.method "POST" $ do
                    H.p $ do label ! A.for "rate1" $ "From eth0 to eth1: "
                             H.select ! A.id "rate1" ! name "new_rate1" ! ralign $ rates
                             label ! A.for "delay1" $ " Delay: "
                             H.select ! A.id "delay1" ! name "new_delay1" ! ralign $ delays
                    H.p $ do label ! A.for "rate2" $ "From eth1 to eth0: "
                             H.select ! A.id "rate1" ! name "new_rate2" ! ralign $ rates
                             label ! A.for "delay2" $ " Delay: "
                             H.select ! A.id "delay2" ! name "new_delay2" ! ralign $ delays
                    H.p $ input ! type_ "submit" ! value "Apply"
        where
          rates = do H.option   "32kbit" ! value   "32kbit"
                     H.option   "64kbit" ! value   "64kbit"
                     H.option  "128kbit" ! value  "128kbit"
                     H.option  "256kbit" ! value  "256kbit"
                     H.option  "512kbit" ! value  "512kbit"
                     H.option "1024kbit" ! value "1024kbit"
          delays = do H.option   "10ms"  ! value   "10ms"
                      H.option   "40ms"  ! value   "40ms"
                      H.option  "160ms"  ! value  "160ms"
                      H.option  "320ms"  ! value  "320ms"
                      H.option  "640ms"  ! value  "640ms"
          ralign = A.style "text-align:right"
          lookCookieValue' d b = fromMaybe d <$> (optional $ lookCookieValue b)

      updateNetem :: ServerPart Response
      updateNetem =
          do method POST
             rate1  <- lookText "new_rate1"
             rate2  <- lookText "new_rate2"
             delay1 <- lookText "new_delay1"
             delay2 <- lookText "new_delay2"
             addCookies [ (Session, mkCookie "rate1"  (unpack rate1))
                        , (Session, mkCookie "rate2"  (unpack rate2))
                        , (Session, mkCookie "delay1" (unpack delay1))
                        , (Session, mkCookie "delay2" (unpack delay2))
                        ]
             seeOther ("/netem" :: String) (toResponse ())
