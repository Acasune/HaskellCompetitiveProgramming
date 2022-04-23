{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.List
import Data.Maybe
import Text.Printf

data Suite = S | H | C | D deriving (Eq, Read, Ord, Show)

data Card = Card Suite Int deriving (Eq, Read, Ord)

instance Show Card where
  show (Card s v) = show s ++ " " ++ show v

process [] [] = return ()
process [] (d1 : allDeck) = do
  print d1
  process [] allDeck
process (d0 : deck) (d1 : allDeck) =
  if d0 /= d1
    then do
      print d1
      process (d0 : deck) allDeck
    else process deck allDeck

main = do
  n <- readInt1 <$> BS.getLine
  ss <- replicateM n getLine
  let deck = strsToDeck ss
  let sortedDeck = sort deck
  let allDeck = [Card a b | a <- [S, H, C, D], b <- [1 .. 13]]
  process sortedDeck allDeck

strsToDeck = map (tran . words)
  where
    tran [a, b] = Card (read a) (read b)

-- -- -- --
putIntN :: [Int] -> IO ()
putIntN = putStrLn . intercalate [' '] . map show

readInt1 :: BS.ByteString -> Int
readInt1 = fst . fromJust . BS.readInt

readInt2 :: BS.ByteString -> (Int, Int)
readInt2 = toTuple . readIntN

readInt3 :: BS.ByteString -> (Int, Int, Int)
readInt3 = toTriple . readIntN

readIntN :: BS.ByteString -> [Int]
readIntN = map readInt1 . BS.words

readInt641 :: BS.ByteString -> Int64
readInt641 = fromIntegral . fst . fromJust . BS.readInteger

readInt642 :: BS.ByteString -> (Int64, Int64)
readInt642 = toTuple . readInt64N

readInt643 :: BS.ByteString -> (Int64, Int64, Int64)
readInt643 = toTriple . readInt64N

readInt64N :: BS.ByteString -> [Int64]
readInt64N = map readInt641 . BS.words

readInteger1 :: BS.ByteString -> Integer
readInteger1 = fst . fromJust . BS.readInteger

readInteger2 :: BS.ByteString -> (Integer, Integer)
readInteger2 = toTuple . readIntegerN

readInteger3 :: BS.ByteString -> (Integer, Integer, Integer)
readInteger3 = toTriple . readIntegerN

readIntegerN :: BS.ByteString -> [Integer]
readIntegerN = map readInteger1 . BS.words

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

toTriple :: [a] -> (a, a, a)
toTriple [x, y, z] = (x, y, z)

fromTuple :: (a, a) -> [a]
fromTuple (x, y) = [x, y]

fromTriple :: (a, a, a) -> [a]
fromTriple (x, y, z) = [x, y, z]

applyTuple :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
applyTuple f g (x, y) = (f x, g y)

applyTriple :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
applyTriple f g h (x, y, z) = (f x, g y, h z)
