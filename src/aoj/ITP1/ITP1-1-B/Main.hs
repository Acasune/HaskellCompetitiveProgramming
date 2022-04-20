module Main where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

printList xs = putStrLn $ unwords . map show $xs

main :: IO ()
main = (readLn :: IO Int) >>= print . (^ 3)
