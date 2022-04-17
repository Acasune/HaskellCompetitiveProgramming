module Main where

import Control.Monad
import Data.Array.IO
import Data.Bool (Bool (False))
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (Foldable (length))
import Data.List
import Data.Maybe

main :: IO ()
main = do
  [n, k] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  a <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ fromJust $ans a k

ans :: [Int] -> Int -> Maybe Int
ans [a] k = return 0
ans (a : as) k = do
  let cnt = length $filter (\x -> x + a <= k) as
  cnt2 <- ans as k
  return (cnt + cnt2)
