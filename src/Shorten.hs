module Shorten where

import Numeric (readHex)
import qualified Data.Char as Char

import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Bases as B

baseUrl :: String
baseUrl = "http://vise890.github.io/"

type URL = String
type MD5HASH = Int
type ShortUnicode = String

hash :: URL -> MD5HASH
hash url = abs n
           where md5 = show . MD5.md5 . LBS.pack
                 n = fst . head . readHex $ md5 url

toShortUnicode :: MD5HASH -> ShortUnicode
toShortUnicode n = map toChar codePoints
                   where n_unicode = (B.newNumber n 16) `B.toBase` 80 -- use less than the whole range for prettiness/emojiRange
                         codePoints = B.digits n_unicode
                         firstEmoji = fst . head $ readHex "1f600"

                         -- shift by at least 21 to avoid space and tab and such
                         -- here, shifting to begin from the first emoji
                         toChar = Char.chr . (+firstEmoji) 

shorten :: URL -> URL
shorten url = baseUrl ++ toShortUnicode (Shorten.hash url)
