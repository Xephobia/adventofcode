import Control.Monad (replicateM)
import GHC.Num (integerLogBase)
import System.Environment (getArgs)

data Token a = Number a | Plus | Mul | Concat deriving (Show, Eq)

intersperseL :: [a] -> [a] -> [a]
intersperseL [] ys = ys
intersperseL _ [] = []
intersperseL (x : xs) (y : ys) = y : x : intersperseL xs ys

fld :: (Token Integer, Maybe (Token Integer)) -> Token Integer -> (Token Integer, Maybe (Token Integer))
fld (n@(Number _), Nothing) Plus = (n, Just Plus)
fld (n@(Number _), Nothing) Mul = (n, Just Mul)
fld (n@(Number _), Nothing) Concat = (n, Just Concat)
fld (Number a, Just Plus) (Number b) = (Number (a + b), Nothing)
fld (Number a, Just Mul) (Number b) = (Number (a * b), Nothing)
fld (Number a, Just Concat) (Number b) = (Number (10 ^ (integerLogBase 10 b + 1) * a + b), Nothing)

carteProd :: Int -> [a] -> [[a]]
carteProd = replicateM

isNumber :: Token a -> Bool
isNumber (Number _) = True
isNumber _ = False

allOpsChain :: [Token Integer] -> Integer -> [Integer] -> [[Token Integer]]
allOpsChain ops n ns =
  map (filter (not . isNumber)) $
    filter (\(nu : o : r) -> fst (foldl fld (nu, Just o) r) == Number n) $
      map (`intersperseL` map Number ns) $
        carteProd (length ns - 1) ops

getPuzzleInput :: FilePath -> IO [(Integer, [Integer])]
getPuzzleInput path = do
  raw <- readFile path
  let (ns, rs) = unzip $ map (span (/= ':')) $ lines raw
  return $ zipWith (\x y -> (read x, map read $ words $ drop 2 y)) ns rs

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  input <- getPuzzleInput inputPath
  print $
    sum $
      map fst $
        filter (not . null . uncurry (allOpsChain [Plus, Mul])) input
  print $
    sum $
      map fst $
        filter (not . null . uncurry (allOpsChain [Plus, Mul, Concat])) input
