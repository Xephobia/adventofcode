import Control.Monad.Trans.State (State, evalState, get, put)
import Data.List (find, nub)
import System.Environment (getArgs)
import Prelude hiding (Left, Right)

data Orientation = Up | Right | Bottom | Left deriving (Show)

type Coordinate = (Int, Int)

type Guard = (Coordinate, Orientation)

-- (obstactles, walked, guardState)
type GuardTour = ([Coordinate], Guard)

turnRight :: Orientation -> Orientation
turnRight o = case o of
  Up -> Right
  Right -> Bottom
  Bottom -> Left
  Left -> Up

pairwiseSum :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
pairwiseSum (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

guardWalk :: Coordinate -> [Coordinate] -> State GuardTour Int
guardWalk bounds@(bx, by) os = do
  (wks, (gc, go)) <- get
  let nextCoord = pairwiseSum gc $ delta go
  let (wks', g) =
        if nextCoord `elem` os
          then (wks, (gc, turnRight go))
          else (nextCoord : wks, (nextCoord, go))
  if outsideBounds nextCoord
    then return $ length $ nub wks
    else do
      put (wks', g)
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
  (dims, obs, g) <- getPuzzleInput inputPath
  print $ evalState (guardWalk dims obs) ([], g)
