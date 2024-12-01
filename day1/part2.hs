import Common
import qualified Data.Map.Lazy as Map
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

countInList :: (Ord a, Num b) => [a] -> Map.Map a b
countInList ls = Map.fromListWith (+) $ map tupleOne ls
  where
    tupleOne x = (x, 1)

similarityScoreLists :: (Num a, Ord a) => [a] -> [a] -> a
similarityScoreLists l r = sum $ mapMaybe similarityScore l
  where
    counted = countInList r
    similarityScore x = (* x) <$> Map.lookup x counted

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  input <- getPuzzleInput inputPath
  putStr $ show $ uncurry similarityScoreLists input
