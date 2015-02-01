module Shorten where

import Numeric (readHex)
import qualified Data.Char as Char

import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Bases as B

-- FIXME: I should really get a server up somewhere...
baseUrl :: String
baseUrl = "http://vise890.github.io/"

type URL = String
type MD5HASH = Int
type ShortUnicode = String

hash :: URL -> MD5HASH
hash url = abs . fst . head $ n
           where md5 = show . MD5.md5 . LBS.pack
                 n = readHex $ md5 url

toShortUnicode :: MD5HASH -> ShortUnicode
toShortUnicode n = map toChar codePoints
                   where n_unicode = (B.newNumber n 16) `B.toBase` 80 -- use less than the whole range for prettiness
                         codePoints = B.digits n_unicode
                         firstEmoji = fst . head $ readHex "1f600"
                         toChar = Char.chr . (+firstEmoji) -- shift by 21 to avoid space and tab and such, this needs to be way more robust

shorten :: URL -> URL
shorten url = baseUrl ++ toShortUnicode (Shorten.hash url)

-- FIXME: be able to see type, instances of the type in the editor
-- FIXME: be able to search hoogle, from the editor
-- I shouldn't have to use Shorten.hash. bringing in MD5 as a qualified import should not interfere with my namespace.
