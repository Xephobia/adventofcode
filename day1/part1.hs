import Common
import Data.List (sort)
import System.Environment (getArgs)

-- Pair up the smallest number in the left list with the smallest number in the right list, then the second-smallest left number with the second-smallest right number, and so on.
-- Within each pair, figure out how far apart the two numbers are; you'll need to add up all of those distances. For example, if you pair up a 3 from the left list with a 7 from the right list, the distance apart is 4; if you pair up a 9 with a 3, the distance apart is 6.

sumOfDistances :: (Ord a, Num a) => [a] -> [a] -> a
sumOfDistances l r = sum $ zipWith (curry $ abs . uncurry (-)) (sort r) (sort l)

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  input <- getPuzzleInput inputPath
  putStr $ show $ uncurry sumOfDistances input
