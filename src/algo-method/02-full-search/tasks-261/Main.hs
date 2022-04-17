module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- getLine
  print $ fromJust $ takes s

takes :: String -> Maybe Int
takes [a] = return 0
takes (a : as) = do
  let cnt = sum [1 | b <- as, a == b]
  cnt1 <- takes as
  return (cnt + cnt1)
