import Control.Applicative
import Control.Monad
import Data.Array
import Data.Array.ST
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import Text.Printf

calc :: Int -> Int -> Int -> Char
calc m f r
  | (m == -1) || (f == -1) = 'F'
  | sum >= 80 = 'A'
  | sum >= 65 = 'B'
  | sum >= 50 = 'C'
  | sum >= 30 && r >= 50 = 'C'
  | sum >= 30 = 'D'
  | otherwise = 'F'
  where
    sum = m + f

main :: IO ()
main = do
  [m, f, r] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  if m == -1 && f == -1 && r == -1
    then return ()
    else do
      putChar $ calc m f r
      putChar '\n'
      main

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
