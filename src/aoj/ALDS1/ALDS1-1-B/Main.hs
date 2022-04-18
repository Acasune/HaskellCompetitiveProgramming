module Main where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  [a, b] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  print $ gcd a b

gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)
