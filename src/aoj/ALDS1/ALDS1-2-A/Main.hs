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
  let (a, k) = bubbleSort [] ss 0
  printList $ a
  print k

bubbleSort :: [Int] -> [Int] -> Int -> ([Int], Int)
bubbleSort sv [] n = (sv, n)
bubbleSort sv xs n =
  let (a : as, b, k) = internal [] xs n
   in bubbleSort (a : sv) (reverse as) k

internal :: [Int] -> [Int] -> Int -> ([Int], [Int], Int)
internal [] (x : xs) n = internal [x] xs n
internal xs [] n = (xs, [], n)
internal (p : ps) (x : xs) n =
  if x >= p
    then internal (x : p : ps) xs n
    else internal (p : x : ps) xs (n + 1)
