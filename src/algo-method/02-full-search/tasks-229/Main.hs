module Main where

import Control.Monad
import Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  s <- getLine :: IO String
  t <- getLine :: IO String
  let ans = if isHas0 s t then "Yes" else "No"
  putStrLn ans

isHas _ [] = True
isHas (s : ss) (t : ts)
  | s == t = isHas ss ts
  | s /= t = False

isHas0 [] _ = False
isHas0 (s : ss) ts
  | isHas (s : ss) ts = True
  | otherwise = isHas0 ss ts
