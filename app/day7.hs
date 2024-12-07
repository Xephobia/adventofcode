import Control.Monad (replicateM)
import System.Environment (getArgs)

data Token a = Number a | Plus | Mul deriving (Show, Eq)

intersperseL :: [a] -> [a] -> [a]
intersperseL [] ys = ys
intersperseL _ [] = []
intersperseL (x : xs) (y : ys) = y : x : intersperseL xs ys

fld :: (Num a) => (Token a, Maybe (Token a)) -> Token a -> (Token a, Maybe (Token a))
fld (n@(Number _), Nothing) Plus = (n, Just Plus)
fld (n@(Number _), Nothing) Mul = (n, Just Mul)
fld ((Number a), Just Plus) (Number b) = (Number (a + b), Nothing)
fld ((Number a), Just Mul) (Number b) = (Number (a * b), Nothing)

carteProd :: Int -> [a] -> [[a]]
carteProd = replicateM

isNumber :: Token a -> Bool
isNumber (Number _) = True
isNumber _ = False

allOpsChain :: (Num a, Eq a) => a -> [a] -> [[Token a]]
allOpsChain n ns =
  map (filter (not . isNumber)) $
    filter (\(nu : o : r) -> fst (foldl fld (nu, Just o) r) == Number n) $
      map (`intersperseL` map Number ns) $
        carteProd (length ns - 1) [Plus, Mul]

getPuzzleInput :: FilePath -> IO [(Int, [Int])]
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
        filter (not . null . uncurry allOpsChain) input
