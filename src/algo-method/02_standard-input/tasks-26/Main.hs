module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

main :: IO ()
main = do
  a <- getLine
  b <- getLine
  c <- getLine
  putStrLn $ c ++ b ++ a
