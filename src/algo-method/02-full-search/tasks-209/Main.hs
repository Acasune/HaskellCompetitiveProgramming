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
  if length (filter (== v) a) > 0 then putStrLn "Yes" else putStrLn "No"
