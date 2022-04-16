module Main where

import Control.Monad
import Data.Array.IO
import Data.Bool (otherwise)
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  let a = [1 .. n]
  fnc a

fnc :: [Int] -> IO ()
fnc [] = return ()
fnc (x : xs)
  | ((x `mod` 3 == 0) && (x `mod` 5 == 0)) = do
    putStrLn "FizzBuzz"
    fnc xs
  | (x `mod` 3 == 0) = do
    putStrLn "Fizz"
    fnc xs
  | (x `mod` 5 == 0) = do
    putStrLn "Buzz"
    fnc xs
  | otherwise = do
    print x
    fnc xs
