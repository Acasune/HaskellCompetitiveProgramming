module Main where

import Control.Monad
import Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  s <- getLine
  print $cnt s 0

cnt :: String -> Int -> Int
cnt [a] n = n
cnt (a : b : c) n
  | a == b = cnt (b : c) (n + 1)
  | otherwise = cnt (b : c) n
