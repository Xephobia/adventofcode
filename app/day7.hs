import GHC.Num (integerLogBase)
import System.Environment (getArgs)
import Control.Monad (guard)

data Op = Plus | Mul | Concat deriving (Show, Eq)

apply :: Integer -> Integer -> Op -> Integer
apply a b Plus = a + b
apply a b Mul  = a * b
apply a b Concat  = 10 ^ (integerLogBase 10 b + 1) * a + b

strategic :: [Op] -> Integer -> [Integer] -> [()]
strategic ops target (x:xs) = strategic' x xs
  where strategic' x' (y:ys) = do result <- apply x' y <$> ops
                                  guard (result <= target)
                                  strategic' result ys
        strategic' x' [] = guard (x' == target)

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
        filter (not . null . uncurry (strategic [Plus,Mul])) input
  print $
    sum $
      map fst $
        filter (not . null . uncurry (strategic [Plus,Mul, Concat])) input
