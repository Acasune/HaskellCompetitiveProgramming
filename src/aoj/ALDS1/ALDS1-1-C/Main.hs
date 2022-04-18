module Main where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- replicateM n readLn
  print $ length $ filter (\x -> isPrime x) ss

isPrime :: Int -> Bool
isPrime x =
  if x == 2
    then True
    else f 2 x
  where
    f b x =
      if (b * b > x)
        then True
        else
          if (x `mod` b) == 0
            then False
            else f (b + 1) x
