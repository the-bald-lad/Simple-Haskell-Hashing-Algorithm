module Main where

import Numeric                      (showHex)
import Data.Text.Encoding           (encodeUtf8)
import System.Random                (randomRIO)
import Control.Monad                (replicateM)
import qualified Data.ByteString    as BS
import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.Text          as T

bytestr :: String -> BS.ByteString
bytestr = encodeUtf8 . T.pack

randomString :: Int -> IO String
randomString len = replicateM len $ randomRIO ('a', '~')

select :: (Num f, Eq f) => [f] -> String -> String
select = zipWith (\f c -> if f == 1 then c else ' ')

removeSpace :: String -> String
removeSpace = foldr go ""
  where
    go x acc = x:if x == ' ' then dropWhile (' ' ==) acc else acc

main :: IO ()
main = do
    putStrLn "Enter a word: "   
    inp       <- getLine
    randWords <- select [1 :: Int,0,1,0,1] <$> randomString 5
    let file     = "hashes.txt"
        salt     = removeSpace randWords
        merge    = inp ++ salt
        bytes    = bytestr merge
        bs       = SHA.hash bytes
        hash     = concatMap (`showHex` "") $ BS.unpack bs
    writeFile file hash
    putStrLn hash