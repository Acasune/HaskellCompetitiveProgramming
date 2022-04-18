module Main where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- replicateM n (fst . fromJust . BS.readInt <$> (BS.getLine))
  print $ f ss

f :: [Int] -> Int
f xs =
  maximum ys
  where
    ys = zipWith (-) (tail xs) (scanl1 min xs)
