module Main where

import Control.Monad
import Control.Monad (forM)
import Data.Array.IO
import Data.Bool (Bool (False))
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (Foldable (length))
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- getLine
  a <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ fromJust $takes a

takes [a, b] = return 0
takes (a : as) = do
  cnt <- ans (a : as)
  cnt1 <- takes as
  return (cnt + cnt1)

ans :: [Int] -> Maybe Int
ans [a] = return 0
ans (a : b : as) = do
  let cnt = if a > b then 0 else sum [1 | c <- as, c <= b]
  cnt1 <- ans (a : as)
  return (cnt + cnt1)
