module Main where

import Control.Monad
import Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  s <- getLine
  let rev = reverse s
  putStrLn $ isSame s rev

isSame :: String -> String -> String
isSame [] [] = "Yes"
isSame (s : ss) (t : ts)
  | s == t = isSame ss ts
  | otherwise = "No"
