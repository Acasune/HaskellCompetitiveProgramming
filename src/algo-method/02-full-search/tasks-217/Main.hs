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
  v <- newArray (1, 10) 0 :: IO (IOArray Int Int)

  forM_ a $ \val -> do
    i <- readArray v val
    writeArray v val (i + 1)

  forM_ [1 .. 9] $ \i -> do
    val <- readArray v i
    print val
