import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Math.LinearEquationSolver (Solver (Z3), solveIntegerLinearEqs)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP

type Coeffs = (Integer, Integer)

type Target = (Integer, Integer)

type ClawMachine = (Coeffs, Coeffs, Target)

clawMachine :: ReadP ClawMachine
clawMachine = do
  string "Button A: X+"
  ax <- munch1 isDigit
  string ", Y+"
  ay <- munch1 isDigit
  string "\nButton B: X+"
  bx <- munch1 isDigit
  string ", Y+"
  by <- munch1 isDigit
  string "\nPrize: X="
  tx <- munch1 isDigit
  string ", Y="
  ty <- munch1 isDigit
  return ((read ax, read ay), (read bx, read by), (read tx, read ty))

machines :: ReadP [ClawMachine]
machines = clawMachine `sepBy` string "\n\n"

getPuzzleInput :: FilePath -> IO [ClawMachine]
getPuzzleInput p = do
  raw <- readFile p
  return $ head [x | (x, _) <- readP_to_S (machines <* optional (char '\n') <* eof) raw]

solve :: ClawMachine -> IO (Maybe Coeffs)
solve ((ax, ay), (bx, by), (tx, ty)) = (fit <$>) <$> solveIntegerLinearEqs Z3 [[ax, bx], [ay, by]] [tx, ty]
  where
    fit [na, nb] = (na, nb)

main :: IO ()
main = do
  (ip : _) <- getArgs
  i <- getPuzzleInput ip
  ss <- map (maybe 0 (\(na, nb) -> 3 * na + nb)) <$> mapM solve i
  print $ sum ss
  sss <- map (maybe 0 (\(na, nb) -> 3 * na + nb)) <$> mapM (solve . (\(a, b, (tx, ty)) -> (a, b, (tx + 10000000000000, ty + 10000000000000)))) i
  print $ sum sss
