module Main where

import Numeric (showHex)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Text as T

bytestr :: String -> BS.ByteString
bytestr = encodeUtf8 . T.pack

main :: IO ()
main = do
    putStrLn "Enter a word: "
    s <- getLine
    let b  = bytestr s
        bs = SHA1.hash b

    putStrLn $ concatMap (`showHex` "") $ BS.unpack bs