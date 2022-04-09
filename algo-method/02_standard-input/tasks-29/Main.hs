module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

main :: IO ()
main = do
  [a, b, c, d] <- map (fst . fromJust . BS.readInteger) . BS.words <$> BS.getLine
  print $ max (max a b) (max c d)
