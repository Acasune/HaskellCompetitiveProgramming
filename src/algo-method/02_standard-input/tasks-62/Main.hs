module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- replicateM n (getLine)
  let cnt = length $ filter (== "left") ss
  if 2 * cnt < n then putStrLn "right" else if 2 * cnt == n then putStrLn "same" else putStrLn "left"
