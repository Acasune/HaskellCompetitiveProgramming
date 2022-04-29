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
import Data.Sequence ((<|),(|>), ViewR(..),ViewL(..), Seq)
import qualified Data.Sequence as Sq
import Text.Printf

main :: IO ()
main = do
  n <- readLn
  replicateM n (readIntN <$> BS.getLine) >>= solve

solve :: [[Int]] -> IO ()
solve qs = foldM f Sq.empty qs >> return ()
  where
    f sq [0,0, x] = pushFront x sq
    f sq [0,1, x] = pushBack sq x
    f sq [1, p] = do
      randomAccess sq p >>= print
      return sq
    f sq [2,0] = popFront sq
    f sq [2,1] = popBack sq

  
-- -- -- --
randomAccess :: Seq Int -> Int -> IO Int
randomAccess sq p = return $ Sq.index sq p

pushBack :: Seq Int -> Int -> IO (Seq Int)
pushBack sq x = return $ sq |> x

popBack :: Seq Int -> IO (Seq Int)
popBack sq = let (sq' :> _) = Sq.viewr sq
             in return sq'

pushFront :: Int -> Seq Int -> IO (Seq Int)
pushFront x sq = return $ x <| sq

popFront :: Seq Int -> IO (Seq Int)
popFront sq = let (_ :< sq') = Sq.viewl sq
             in return sq'
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
