module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

main :: IO ()
main = do
  s <- getLine
  n <- (fst . fromJust . BS.readInt) <$> BS.getLine :: IO Int
  putStrLn $ (s !! (n -1)) : []
