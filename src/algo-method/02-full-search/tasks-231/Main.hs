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
  n <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  a <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  b <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  let ans = sum [1 | i <- a, j <- b, i > j]
  print ans
