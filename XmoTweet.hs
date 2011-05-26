{-# LANGUAGE ScopedTypeVariables #-}
module Plugins.XmoTweet (XmoTweet(..)) where

import Plugins
import Network.HTTP.Wget
import Text.XML.Light.Proc
import Text.XML.Light.Input
import Text.XML.Light.Types
import qualified Control.Exception as E


data XmoTweet = XmoTweet Int
    deriving (Read, Show)

instance Exec XmoTweet where
    alias (XmoTweet _) = "xmotweet"
    run   (XmoTweet _) = getTweet
    rate  (XmoTweet r) = r 

simpleName s = QName s Nothing Nothing

getTweet :: IO String
getTweet = 
  E.catch ( do tweet <- wget "http://api.twitter.com/1/users/show.xml?screen_name=chris10ph" [][]
               let contents = parseXML tweet
               let quotes = concatMap (findElements $ simpleName "text") (onlyElems contents)
               return $ strContent $ head quotes)
          (\(err :: E.SomeException) -> do return "ups!")
