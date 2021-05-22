module Day4 where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5
import Data.Text(chunksOf, pack, unpack)

isValidHash :: String -> Int -> Int -> Bool
isValidHash s x i = (unpack . head . chunksOf x . pack . show . md5 . B.pack $ s ++ show i) == replicate x '0'

getValidHash :: String -> Int -> Int
getValidHash s x = head $ filter (isValidHash s x) [0..]

runDay4 = do
    print $ getValidHash "bgvyzdsv" 5
    print $ getValidHash "bgvyzdsv" 6
