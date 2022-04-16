module Main where

import Control.Monad
import Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  print $ length $ filter (\x -> (x `mod` 2 /= 0) && (x `mod` 3 /= 0) && (x `mod` 5 /= 0)) [1 .. n]
