module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

main :: IO ()
main = do
  s <- BS.getLine
  lst <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ foldl1 (+) lst
