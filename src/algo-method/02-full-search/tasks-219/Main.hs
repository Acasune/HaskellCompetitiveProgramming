module Main where

import Control.Monad
import Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- BS.getLine
  a <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  let b = snd $ head $ reverse $sort $ fn a
  print $ b

fn :: [Int] -> [(Int, Int)]
fn = map (\e -> (length e, head e)) . group . sort
