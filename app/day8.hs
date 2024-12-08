import Data.Bifunctor (second)
import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)

newtype Pt = Pt (Int, Int) deriving (Ord, Eq, Show)

instance Num Pt where
  (Pt (a1, b1)) + (Pt (a2, b2)) = Pt (a1 + a2, b1 + b2)
  (Pt (a1, b1)) * (Pt (a2, b2)) = Pt (a1 * a2, b1 * b2)
  abs (Pt (a, b)) = Pt (abs a, abs b)
  signum (Pt (a, b)) = Pt (signum a, signum b)
  fromInteger i = Pt (fromInteger i, fromInteger i)
  negate (Pt (a, b)) = Pt (-a, -b)

getPuzzleInput :: FilePath -> IO (Pt, Map Char (Set Pt))
getPuzzleInput path = do
  raw <- readFile path
  let ls = lines raw
  let dims = Pt (length $ head ls, length ls)
  let cds =
        concatMap (filter (isAlphaNum . fst)) $
          zipWith (\l y -> zipWith (\c x -> (c, Pt (x, y))) l [0 ..]) ls [0 ..]
  return (dims, Map.fromListWith Set.union $ map (second Set.singleton) cds)

placeAntinodesModel :: (Pt -> Pt -> Seq Pt) -> Map a (Set Pt) -> Set Pt
placeAntinodesModel atnPt fs =
  Set.fromList $
    toList $
      foldl1 (><) $
        Map.elems $
          Map.map (atnSeq . Seq.fromList . Set.toList) fs
  where
    withAll :: (a -> a -> b) -> Seq a -> Seq (Seq b)
    withAll f sqs = Seq.mapWithIndex (\n e -> f e <$> Seq.deleteAt n sqs) sqs
    atnSeq :: Seq Pt -> Seq Pt
    atnSeq sqs = foldl1 (><) $ foldl1 (><) $ withAll atnPt sqs

placeAntinodes :: Map a (Set Pt) -> Set Pt
placeAntinodes = placeAntinodesModel atnPt
  where
    atnPt pa pb = Seq.fromList $ zipWith (+) [pa, pa] [-pd, Pt (2, 2) * pd]
      where
        pd = pb - pa

withinBounds :: Pt -> Pt -> Bool
withinBounds (Pt (da, db)) (Pt (a, b)) = all (>=0) [a, b] && a < da && b < db

placeAntinodesResonant :: Pt -> Map a (Set Pt) -> Set Pt
placeAntinodesResonant dims = placeAntinodesModel atnPt
  where
    atnPt pa pb = Seq.fromList $ pos ++ neg
      where
        pd = pb - pa
        pos = takeWhile (withinBounds dims) $ map (\x -> pa+Pt(x,x)*pd) [1..]
        neg = takeWhile (withinBounds dims) $ map (\x -> pa+Pt(x,x)*pd) [-1,-2..]
        


main :: IO ()
main = do
  (inputPath : _) <- getArgs
  (dims, input) <- getPuzzleInput inputPath
  print $ length $ filter (withinBounds dims) $ toList $ placeAntinodes input
  print $ length $ filter (withinBounds dims) $ toList $ placeAntinodesResonant dims input
