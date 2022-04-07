module Main where

main :: IO ()
main = do
  x <- readLn :: IO Int
  print $ 24 - x
