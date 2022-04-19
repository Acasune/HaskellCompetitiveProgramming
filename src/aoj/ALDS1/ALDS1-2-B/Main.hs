module Main where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

printList xs = putStrLn $ unwords . map show $xs

main :: IO ()
main = do
  _ <- readLn :: IO Int
  ss <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  let (a, k) = sSort [] ss 0
  printList $reverse a
  print k

sSort :: [Int] -> [Int] -> Int -> ([Int], Int)
sSort sv [x] n = (x : sv, n)
sSort sv (x : y : xs) n =
  let (a, b, z) = internal [] [] xs y
   in if z < x
        then sSort (z : sv) (a ++ [x] ++ b) (n + 1)
        else sSort (x : sv) (a ++ [z] ++ b) n

internal :: [Int] -> [Int] -> [Int] -> Int -> ([Int], [Int], Int)
internal left centor [] cand = (left, centor, cand)
internal left centor (r : right) cand =
  if r >= cand
    then internal left (centor ++ [r]) right cand
    else internal (left ++ [cand] ++ centor) [] right r
