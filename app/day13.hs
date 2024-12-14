import Data.Char (isDigit)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP

type Coeffs = (Int, Int)

type Target = (Int, Int)

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

solve :: ClawMachine -> [Coeffs]
solve ((ax, ay), (bx, by), (tx, ty)) = [(na, nb) | na <- [0 .. 100], nb <- [0 .. 100], na * ax + nb * bx == tx, na * ay + nb * by == ty]

main :: IO ()
main = do
  (ip : _) <- getArgs
  i <- getPuzzleInput ip
  print $ sum $ map ((\(na, nb) -> 3 * na + nb) . head) $ filter (not . null) $ map solve i
