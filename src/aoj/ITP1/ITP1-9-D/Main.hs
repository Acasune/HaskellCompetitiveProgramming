import Control.Applicative
import Control.Monad
import Data.Array
import Data.Array.ST
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Int (Int64)
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import Text.Printf

procedure _ _ 0 = return ()
procedure (op : ops) s n = do
  let p = op !! 0
  let l = read (op !! 1)
  let r = read (op !! 2)
  let (ls, rem) = splitAt l s
  let (internal, rs) = splitAt (r - l + 1) rem
  case p of
    "replace" -> do
      procedure ops (ls ++ (op !! 3) ++ rs) (n -1)
    "reverse" -> do
      procedure ops (ls ++ (reverse internal) ++ rs) (n -1)
    "print" -> do
      putStrLn internal
      procedure ops s (n -1)

main :: IO ()
main = do
  s <- getLine
  n <- readLn
  op <- replicateM n getLine
  let ops = map (words) op
  procedure ops s n

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
