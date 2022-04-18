module Main where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

list_to_string = unwords . map show

main :: IO ()
main = do
  n <- readLn :: IO Int
  a <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  insertSort (head a) [] (tail a)

insertSort :: Int -> [Int] -> [Int] -> IO ()
insertSort a bf [] = do
  let tmp = sort (bf ++ [a])
  putStrLn $ list_to_string $tmp
insertSort a bf af = do
  let tmp = sort (bf ++ [a])
  putStrLn $ list_to_string $tmp ++ af
  insertSort (head af) tmp (tail af)
