module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

main :: IO ()
main = do
  s <- getLine
  [n1, m1] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine

  let n = n1 -1
  let m = m1 -1
  putStrLn $ (slice 0 (n -1) s) ++ [(s !! m)] ++ (slice (n + 1) (m -1) s) ++ [(s !! n)] ++ (slice (m + 1) ((length s) -1) s)
