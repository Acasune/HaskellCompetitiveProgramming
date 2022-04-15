module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

main :: IO ()
main = do
  [n, v] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  a <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

  let b = reverse a
  let c = length $takeWhile (/= v) b

  if c == n then print (-1) else print (n - c -1)
