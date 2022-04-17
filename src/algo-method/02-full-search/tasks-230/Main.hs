module Main where

import Control.Monad
import Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- getLine
  t <- getLine
  print $cnt s t 0

cnt :: String -> String -> Int -> Int
cnt [] [] n = n
cnt (s : ss) (t : ts) n
  | s == t = cnt ss ts n
  | otherwise = cnt ss ts n + 1
