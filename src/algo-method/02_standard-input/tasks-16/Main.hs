module Main where

main :: IO ()
main = do
  n <- readLn :: IO Int
  print$ mod n 5
