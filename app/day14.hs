{-# LANGUAGE DeriveGeneric #-}

import Control.Lens.Each (each)
import Control.Lens.Fold (foldl1Of)
import Control.Monad (zipWithM_)
import Control.Monad.Trans.State.Strict (State, evalState, get, put, runState)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.HashSet (HashSet)
import qualified Data.HashSet as HSet
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, char, eof, munch1, option, optional, readP_to_S, sepBy1, string)

newtype Pt = Pt (Int, Int) deriving (Ord, Eq, Show, Generic)

instance Hashable Pt

instance Num Pt where
  (Pt (a1, b1)) + (Pt (a2, b2)) = Pt (a1 + a2, b1 + b2)
  (Pt (a1, b1)) * (Pt (a2, b2)) = Pt (a1 * a2, b1 * b2)
  abs (Pt (a, b)) = Pt (abs a, abs b)
  signum (Pt (a, b)) = Pt (signum a, signum b)
  fromInteger i = Pt (fromInteger i, fromInteger i)
  negate (Pt (a, b)) = Pt (-a, -b)

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

showPts :: HashSet Pt -> String
showPts sp = replicate dimX '=' ++ '\n' : concatMap (\y -> map (\x -> if HSet.member (Pt (x, y)) sp then '#' else ' ') [0 .. dimX - 1] ++ ['\n']) [0 .. dimY - 1] ++ '\n' : replicate dimX '='

values :: Int -> State a b -> a -> [b]
values 0 _ _ = []
values n st si = a : values (n - 1) st s
  where
    (a, s) = runState st si

main :: IO ()
main = do
  (ip : _) <- getArgs
  i <- getPuzzleInput ip
  let rs = evalState (foldl1 (>>) $ replicate 100 walk) i
  print $ foldl1Of each (*) $ foldl' count (0, 0, 0, 0) rs
  let a = values 60000 walk i
  zipWithM_ (\n s -> putStrLn $ show n ++ showPts (HSet.fromList s)) [1 ..] a -- search "#####################" in the output, you'll find the tree along with the number at the top of the "frame"
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
