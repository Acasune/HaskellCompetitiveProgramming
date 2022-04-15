module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

main :: IO ()
main = do
  n <- BS.readInt <$> BS.getLine
  a <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

  print $ ret a 0

ret [] n = n
ret (z : []) n = n
ret (x : y : []) n = if x < y then (n + 1) else n
ret (x : y : z) n = if x < y then ret (y : z) (n + 1) else ret (y : z) n
