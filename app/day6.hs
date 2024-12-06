import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Prelude hiding (Left, Right)

data Orientation = Up | Right | Bottom | Left deriving (Show, Ord, Eq)

type Coordinate = (Int, Int)

type Guard = (Coordinate, Orientation)

-- (walked, guardState)
type GuardTour = (Set Guard, Guard)

turnRight :: Orientation -> Orientation
turnRight o = case o of
  Up -> Right
  Right -> Bottom
  Bottom -> Left
  Left -> Up

pairwiseSum :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
pairwiseSum (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

guardWalk :: Coordinate -> [Coordinate] -> State GuardTour (Int, Bool)
guardWalk bounds@(bx, by) os = do
  (wks, (gc, go)) <- get
  let nextCoord = pairwiseSum gc $ delta go
  let (wks', g) =
        if nextCoord `elem` os
          then (wks, (gc, turnRight go))
          else (Set.insert (nextCoord, go) wks, (nextCoord, go))
  let nPoss = Set.size $ Set.map fst wks
  if outsideBounds nextCoord
    then return (nPoss, False)
    else if g `Set.member` wks
         then return (nPoss, True)
         else do put (wks', g)
                 guardWalk bounds os
  where
    outsideBounds (x, y) = x > bx || y > by || any (< 0) [x, y]
    delta o = case o of
      Up -> (0, 1)
      Right -> (1, 0)
      Bottom -> (0, -1)
      Left -> (-1, 0)

getPuzzleInput :: FilePath -> IO (Coordinate, [Coordinate], Guard)
getPuzzleInput path = do
  raw <- readFile path
  let nl = length $ lines raw
  let nx = length $ head $ lines raw
  let cords = concat $ zipWith (\l y -> zipWith (\c x -> (c, (x, y))) l [0 ..]) (lines raw) [nl - 1, nl - 2 .. 0]
  let dims = (nx, nl)
  let obs = map snd $ filter (('#' ==) . fst) cords
  let g = case find ((`elem` ['^', '>', '<']) . fst) cords of
        Just (o, xy) -> case o of
          '^' -> (xy, Up)
          '>' -> (xy, Right)
          '<' -> (xy, Left)
          _ -> error "dragons were met"
        Nothing -> error "guard not found"
  return (dims, obs, g)

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  (dims@(xd, yd), obs, g@((gx, gy),_)) <- getPuzzleInput inputPath
  print $ fst $ evalState (guardWalk dims obs) (Set.empty, g)
  print $ length $ [() | x <- [0..xd-1], y <- [0..yd-1], (x,y) `notElem` obs, (x,y) /= (gx,gy+1), snd $ evalState (guardWalk dims $ (x,y):obs) (Set.empty, g)]
