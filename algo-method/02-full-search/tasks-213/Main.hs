module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

main :: IO ()
main = do
  n <- BS.getLine
  a <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

  print $ last (sort a)
