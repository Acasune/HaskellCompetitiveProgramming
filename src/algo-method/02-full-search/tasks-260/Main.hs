module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- read <$> getLine
  ss <- replicateM n getLine
  let ans = if any (\x -> length x > 1) (group (sort ss)) then "Yes" else "No"
  putStrLn ans
