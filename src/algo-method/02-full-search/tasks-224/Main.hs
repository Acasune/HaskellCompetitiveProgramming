module Main where

import Control.Monad
import Control.Monad (Monad (return))
import Control.Monad.Cont
import Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  [a, b] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  let c = reverse [1 .. a]
  let ans = fnc c a b
  print ans

fnc :: [Int] -> Int -> Int -> Int
fnc (x : xs) a b
  | x == 1 = 1
  | ((a `mod` x == 0) && (b `mod` x == 0)) = x
  | otherwise = fnc xs a b
