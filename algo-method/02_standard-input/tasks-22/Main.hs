module Main where

main :: IO ()
main = do
  s <- getLine :: IO String
  let len = length s :: Int
  putChar $ s !! (len `div` 2)
