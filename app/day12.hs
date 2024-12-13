import Control.Monad.Trans.State.Strict
import Data.Array (Array, bounds, listArray, (!))
import Data.Ix (Ix, inRange, index, range, rangeSize)
import Data.Set (Set, (\\))
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

adj :: Pt -> [Pt]
adj p = map ((p +) . Pt) [(-1, 0), (1, 0), (0, 1), (0, -1)]

type Garden = Array Pt Char

-- set of points in the group, set of points adjacent to the group
type Group = (Set Pt, [Pt])

findRegions :: Garden -> State (Set Pt, [Set Pt]) [Group]
findRegions g = do
  (rs, fr) <- get
  if Set.null rs
    then return (zip fr (map (ar . Set.toList) fr))
    else do
      let r = evalState (takeR $ head $ Set.toList rs) Set.empty
      put (rs \\ r, r : fr)
      findRegions g
  where
    b = bounds g
    ar xs = concatMap (filter (\p -> not (inRange b p && g ! p == ch)) . adj) xs
      where
        ch = g ! head xs
    takeR :: Pt -> State (Set Pt) (Set Pt)
    takeR i = do
      s <- get
      let si = Set.insert i s
      put si
      let s' = execState (mapM takeR $ filter (\p -> p `Set.notMember` s && inRange b p && g ! p == ch) $ adj i) si
      put s'
      return s'
      where
        ch = g ! i

getPuzzleInput :: FilePath -> IO Garden
getPuzzleInput p = do
  raw <- readFile p
  let l = lines raw
  let dims = Pt (length $ head l, length l)
  return $ listArray (Pt (1, 1), dims) $ concat l

main :: IO ()
main = do
  (ip : _) <- getArgs
  i <- getPuzzleInput ip
  let rs = evalState (findRegions i) (Set.fromList (range $ bounds i), [])
  print $ sum $ (\(a, b) -> Set.size a * length b) <$> rs
