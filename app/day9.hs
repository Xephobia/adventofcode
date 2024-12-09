import Data.Char (isDigit)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Sequence (Seq ((:<|), (:|>)), (|>))
import qualified Data.Sequence as Seq
import System.Environment (getArgs)

diskLayout :: [(Int, Int)] -> Seq (Maybe Int)
diskLayout m = Seq.fromList $ concatMap file $ zipWith (\sf i -> (i, sf)) m [0 ..]
  where
    file (i, (s, f)) = replicate s (Just i) ++ replicate f Nothing

diskLayout' :: [(Int, Int)] -> Seq (Seq (Maybe Int))
diskLayout' m = Seq.fromList $ concatMap file $ zipWith (\sf i -> (i, sf)) m [0 ..]
  where
    file (i, (s, f)) = if f == 0 then [Seq.replicate s (Just i)] else [Seq.replicate s (Just i), Seq.replicate f Nothing]

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

forcesIn :: Seq a -> Seq a -> Seq (Seq a)
forcesIn a b = if Seq.null droped then sa else sa |> droped
  where
    droped = Seq.drop (Seq.length a) b
    sa = Seq.singleton a

compactFiles' :: Seq (Seq (Maybe Int)) -> Seq (Maybe Int)
compactFiles' fs = foldl1 (<>) $ cfs fs
  where
    cfs :: Seq (Seq (Maybe Int)) -> Seq (Seq (Maybe Int))
    cfs Seq.Empty = Seq.empty
    cfs (e :<| Seq.Empty) = Seq.singleton e
    cfs (l :|> rs@(r :<| _)) = case r of
      Nothing -> cfs l :|> rs
      Just _ -> case Seq.spanl (\es@(e :<| _) -> not (isNothing e && Seq.length es >= Seq.length rs)) l of
        (fu, fr :<| frs) -> cfs (fu <> forcesIn rs fr <> frs |> Seq.replicate (Seq.length rs) Nothing)
        (fu, Seq.Empty) -> cfs l |> rs

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
  print $ checksum $ compactFiles' $ diskLayout' input
