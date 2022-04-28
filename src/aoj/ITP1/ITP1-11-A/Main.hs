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

data Dice = Dice{tp::Int, bk::Int, rh::Int, lh::Int, fr::Int , bm::Int} deriving(Show, Eq)
initDice::[Int] -> Dice
initDice [tp, bk, rh, lh, fr, bm ] = Dice tp bk rh lh fr bm

roll::Dice->Char->Dice
roll (Dice tp bk rh lh fr bm ) c = case c of
  'E' -> Dice lh bk tp bm fr rh
  'N' -> Dice bk bm rh lh tp fr
  'S' -> Dice fr tp rh lh bm bk
  'W' -> Dice rh bk bm tp fr lh
  otherwise -> Dice tp fr lh rh bk bm

rolls :: Dice->String->Dice
rolls d s= foldl roll d s 

main :: IO ()
main = do
  xs <- readIntN <$> BS.getLine
  ops <- getLine 
  print $ tp$rolls (initDice xs) ops
  

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

readDouble1 :: String -> Double
readDouble1 = fst . fromJust . (read :: String -> Maybe (Double, String))

readDoubleN :: String -> [Double]
readDoubleN = map (read :: String -> Double) . words

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
