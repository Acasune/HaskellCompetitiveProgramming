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
  [l, r] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ fromJust $ans [l .. r]

ans :: [Int] -> Maybe Int
ans [a] = return 0
ans (a : as) = do
  let cnt = length $filter (\x -> (x `mod` 10) == a `mod` 10) as
  cnt2 <- ans as
  return (cnt + cnt2)
