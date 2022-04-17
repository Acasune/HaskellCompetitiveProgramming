module Main where

import Control.Monad
import Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  s <- getLine
  a <- getChar
  let ans = if a `elem` s then "Yes" else "No"
  putStrLn ans
