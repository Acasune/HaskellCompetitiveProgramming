module Main where

import Control.Monad
import Data.Array.IO
import Data.Bool (Bool (False))
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (Foldable (length))
import Data.List
import Data.Maybe

main :: IO ()
main = do
  [n, k] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  let ans = length $ filter (isAns 0 k 1) [1 .. n]
  print ans

isAns :: Int -> Int -> Int -> Int -> Bool
isAns cnt k base n
  | base > n = cnt == k
  | n `mod` base == 0 = isAns (cnt + 1) k (base + 1) n
  | otherwise = isAns cnt k (base + 1) n
