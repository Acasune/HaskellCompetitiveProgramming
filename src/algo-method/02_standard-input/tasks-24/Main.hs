module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

main :: IO ()
main =
  do
    [a, b] <- map (fst . fromJust . BS.readInteger) . BS.words <$> BS.getLine
    print $ a + b
