module Main where

import Control.Monad
import Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  if n == 1
    then putStrLn "No"
    else do
      let flg = (==) (length $ filter (\x -> n `mod` x == 0) [2 .. (n -1)]) 0
      if flg then putStrLn "Yes" else putStrLn "No"
