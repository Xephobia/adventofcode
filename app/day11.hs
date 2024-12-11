import Data.Functor ((<&>))
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import GHC.Num (integerLogBase)
import System.Environment (getArgs)

blink :: (Integral a) => a -> Seq a
blink n
  | n == 0 = Seq.singleton 1
  | even nd = seqify $ n `divMod` (10 ^ (nd `div` 2))
  | otherwise = Seq.singleton $ n * 2024
  where
    seqify (a, b) = Seq.fromList [a, b]
    nd = integerLogBase 10 (toInteger n) + 1

getPuzzleInput :: FilePath -> IO [Integer]
getPuzzleInput p = readFile p <&> (map read . words)

main :: IO ()
main = do
  (p : _) <- getArgs
  i <- getPuzzleInput p
  print $ Seq.length $ blinkNtimes 25 $ Seq.fromList i
  where
    blinkNtimes 0 s = s
    blinkNtimes n s = blinkNtimes (n - 1) $ foldl1 (><) $ blink <$> s
