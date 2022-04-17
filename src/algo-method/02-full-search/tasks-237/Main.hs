module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- replicateM n (getLine)
  print $ sum [1 | s <- ss, fromJust (check s)]

check ss = do
  let revs = reverse ss
  return (isSame ss revs)

isSame :: String -> String -> Bool
isSame [] [] = True
isSame (s : ss) (t : ts)
  | s == t = isSame ss ts
  | otherwise = False
