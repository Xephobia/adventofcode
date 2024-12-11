import Control.Monad.Trans.State.Strict
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import GHC.Num (integerLogBase)
import System.Environment (getArgs)

blinkTimes :: Int -> Int -> State (HashMap (Int, Int) (Seq Int)) (Seq Int)
blinkTimes 0 n' = do
  mem <- get
  put (HM.insert (0, n') (Seq.singleton n') mem) >> return (Seq.singleton n')
blinkTimes 1 n' = do
  mem <- get
  put (HM.insert (1, n') b mem) >> return b
  where
    b = blink n'
blinkTimes n n' = do
  mem <- get
  case mem !? (n, n') of
    Just r -> return r
    Nothing -> do
      let bs = blink n'
      let s = execState (mapM (blinkTimes (n - 1)) bs) mem
      let rs = foldl' (><) Seq.empty $ (\x -> s HM.! (n - 1, x)) <$> bs
      put $ HM.insert (n, n') rs s
      return rs

blink :: (Integral a) => a -> Seq a
blink n
  | n == 0 = pure 1
  | even nd = seqify $ n `divMod` (10 ^ (nd `div` 2))
  | otherwise = pure $ n * 2024
  where
    seqify (a, b) = Seq.fromList [a, b]
    nd = integerLogBase 10 (toInteger n) + 1

getPuzzleInput :: FilePath -> IO [Int]
getPuzzleInput p = readFile p <&> (map read . words)

main :: IO ()
main = do
  (p : _) <- getArgs
  i <- getPuzzleInput p
  let s25 = execState (mapM (blinkTimes 25) i) HM.empty
  print $ sum $ map (\x -> length (s25 HM.! (25, x))) i
  let s75 = execState (mapM (blinkTimes 75) i) s25
  print $ sum $ map (\x -> length (s75 HM.! (75, x))) i
