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
  let revs = map (rev 0) [l .. r]
  print $ comp [l .. r] revs 0

comp [] [] n = n
comp (s : ss) (t : ts) n
  | s == t = comp ss ts n + 1
  | otherwise = comp ss ts n

rev base s
  | s == 0 = base
  | otherwise = do
    let tmp = s `mod` 10
    let tbase = 10 * base + tmp
    let ts = s `div` 10
    rev tbase ts
