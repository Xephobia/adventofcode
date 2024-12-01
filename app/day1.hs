import Data.List (sort)
import System.Environment (getArgs)
import qualified Data.Map.Lazy as Map
import Data.Maybe (mapMaybe)

getPuzzleInput :: FilePath -> IO ([Int],[Int])
getPuzzleInput path = do raw <- readFile path
                         return $ unzip $ map (tuplify2 . (map read . words)) $ lines raw
                           where tuplify2 [x,y] = (x,y)

-- part 1

sumOfDistances :: (Ord a, Num a) => [a] -> [a] -> a
sumOfDistances l r = sum $ zipWith absDiff (sort r) (sort l)
  where absDiff a b = abs (a - b)

-- part 2

similarityScoreLists :: (Num a, Ord a) => [a] -> [a] -> a
similarityScoreLists l r = sum $ mapMaybe similarityScore l
  where
    tupleOne x = (x, 1)
    countInList ls = Map.fromListWith (+) $ map tupleOne ls
    counted = countInList r
    similarityScore x = (* x) <$> Map.lookup x counted

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  input <- getPuzzleInput inputPath
  putStrLn $ show $ uncurry sumOfDistances input
  putStrLn $ show $ uncurry similarityScoreLists input
