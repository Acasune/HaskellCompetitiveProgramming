module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  putStrLn $ if s == t then "Yes" else "No"
