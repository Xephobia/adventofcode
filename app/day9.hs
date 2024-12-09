import Data.Char (isDigit)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import System.Environment (getArgs)

diskLayout :: [(Int, Int)] -> Seq (Maybe Int)
diskLayout m = Seq.fromList $ concatMap file $ zipWith (\sf i -> (i, sf)) m [0 ..]
  where
    file (i, (s, f)) = replicate s (Just i) ++ replicate f Nothing

compactFiles :: Seq (Maybe Int) -> Seq (Maybe Int)
compactFiles fs = cfs fs
  where
    lengthFs = length fs
    cfs fs' =
      if Seq.length ffu + Seq.length ffr == lengthFs
        then ffu <> ffr
        else
          let (lfr, r'') = Seq.spanr isNothing r'
              (lfu, r''') = Seq.spanr isJust r''
              (tfr, tfrs) = Seq.splitAt (Seq.length lfu) ffr
              (tfus, tfu) = Seq.splitAt (max 0 (Seq.length lfu - Seq.length tfr)) lfu
           in cfs $ ffu <> Seq.reverse tfu <> tfrs <> r''' <> tfus <> tfr <> lfr
      where
        (ffu, r) = Seq.spanl isJust fs'
        (ffr, r') = Seq.spanl isNothing r

checksum :: Seq (Maybe Int) -> Int
checksum s = sum $ Seq.mapWithIndex (flip ((*) . fromMaybe 0)) s

getPuzzleInput :: FilePath -> IO [(Int, Int)]
getPuzzleInput path = do
  raw <- readFile path
  return (fld $ filter isDigit raw)
  where
    fld :: String -> [(Int, Int)]
    fld (a : b : r) = (read [a], read [b]) : fld r
    fld [a] = [(read [a], 0)]
    fld [] = []

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  input <- getPuzzleInput inputPath
  print $ checksum $ compactFiles $ diskLayout input
