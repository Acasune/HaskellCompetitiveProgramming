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
  let ans = filter (\x -> x `mod` 3 == 0) lst
  mapM_ print ans
