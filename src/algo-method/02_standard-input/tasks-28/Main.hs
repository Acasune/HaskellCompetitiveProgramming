module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

main :: IO ()
main = do
  [a, b, c] <- map (fst . fromJust . BS.readInteger) . BS.words <$> BS.getLine
  putStrLn $ if a `mod` b == 0 then "Yes" else "No"
