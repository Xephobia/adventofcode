import Data.Array (Array, assocs, bounds, listArray, (!))
import Data.Ix (Ix, inRange, index, range, rangeSize)
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

instance Ix Pt where
  range (Pt (lx, ly), Pt (ux, uy)) = [Pt (x, y) | y <- range (ly, uy), x <- range (lx, ux)]
  index (Pt (lx, ly), Pt (ux, uy)) (Pt (ix, iy)) = index (ly, uy) iy * rangeSize (lx, ux) + index (lx, ux) ix
  inRange (Pt b1, Pt b2) (Pt c) = inRange (b1, b2) c

type TopoMap = Array Pt Int

directions :: TopoMap -> Pt -> [Pt]
directions tm p = filter (\x -> inRange b x && ((tm ! x) - (tm ! p) == 1)) adj
  where
    adj = map ((p +) . Pt) [(-1, 0), (1, 0), (0, -1), (0, 1)]
    b = bounds tm

walk :: TopoMap -> Pt -> Set Pt
walk tm p =
  if tm ! p == 9
    then Set.singleton p
    else
      if null pos
        then Set.empty
        else Set.unions $ map (\p' -> walk tm p') pos
  where
    pos = directions tm p

getPuzzleInput :: FilePath -> IO (TopoMap, [Pt])
getPuzzleInput path = do
  raw <- readFile path
  let l = lines raw
  let xb = length $ head l
  let b = (Pt (0, 0), Pt (xb - 1, max 0 (length l - 1)))
  let tm = listArray b $ map (read . pure) $ concat l
  let ss = map fst $ filter ((== 0) . snd) $ assocs tm
  return (tm, ss)

main :: IO ()
main = do
  (p : _) <- getArgs
  (tm, ss) <- getPuzzleInput p
  print $ sum $ map (Set.size . walk tm) ss
