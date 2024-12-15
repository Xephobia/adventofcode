import Control.Lens.Each (each)
import Control.Lens.Fold (foldl1Of)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Char
import Data.Foldable (foldl')
import Data.Ix
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, eof, munch1, option, optional, readP_to_S, sepBy1, string)

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

type Robot = (Pt, Pt)

dimX :: Int
dimX = 101

dimY :: Int
dimY = 103

walk :: State [Robot] [Pt]
walk = do
  s <- get
  let s' = map (\(p, v) -> (wrap (p + v), v)) s
  put s'
  return $ map fst s'
  where
    wrap (Pt (x, y)) = Pt (x `mod` dimX, y `mod` dimY)

robot :: ReadP Robot
robot = do
  string "p="
  px <- munch1 isDigit
  char ','
  py <- munch1 isDigit
  string " v="
  svx <- option 1 (char '-' >> return (-1))
  vx <- munch1 isDigit
  char ','
  svy <- option 1 (char '-' >> return (-1))
  vy <- munch1 isDigit
  return (Pt (read px, read py), Pt (svx * read vx, svy * read vy))

robots :: ReadP [Robot]
robots = robot `sepBy1` char '\n'

getPuzzleInput :: FilePath -> IO [Robot]
getPuzzleInput p = do
  raw <- readFile p
  return $ head [x | (x, _) <- readP_to_S (robots <* optional (char '\n') <* eof) raw]

main :: IO ()
main = do
  (ip : _) <- getArgs
  i <- getPuzzleInput ip
  let rs = evalState (foldl1 (>>) $ replicate 100 walk) i
  print $ foldl1Of each (*) $ foldl' count (0, 0, 0, 0) rs
  where
    btwn l r a = l <= a && a <= r
    count (n1, n2, n3, n4) (Pt (x, y))
      | btwn 0 ((dimX `div` 2) - 1) x = case () of
          ()
            | btwn 0 ((dimY `div` 2) - 1) y -> (n1 + 1, n2, n3, n4)
            | btwn ((dimY `div` 2) + 1) (dimY - 1) y -> (n1, n2, n3 + 1, n4)
            | otherwise -> (n1, n2, n3, n4)
      | btwn ((dimX `div` 2) + 1) (dimX - 1) x = case () of
          ()
            | btwn 0 ((dimY `div` 2) - 1) y -> (n1, n2 + 1, n3, n4)
            | btwn ((dimY `div` 2) + 1) (dimY - 1) y -> (n1, n2, n3, n4 + 1)
            | otherwise -> (n1, n2, n3, n4)
      | otherwise = (n1, n2, n3, n4)
