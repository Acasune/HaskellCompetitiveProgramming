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
  n <- readLn :: IO Int
  lst <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  let ans = length $ filter (isPrime 2) lst
  print ans

isPrime pos n
  | n == 1 = False
  | n == 2 = True
  | pos * pos > n = True
  | n `mod` pos == 0 = False
  | otherwise = isPrime (pos + 1) n
