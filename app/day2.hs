import System.Environment (getArgs)

getPuzzleInput :: FilePath -> IO [[Int]]
getPuzzleInput path = do raw <- readFile path
                         return $ map (map read . words) $ lines raw

data (Ord a, Num a) => ReportSafety a = Increasing a | Decreasing a | Unknown a | Unsafe
  deriving (Show)

isNotDifferentEnough :: (Ord a, Num a) => a -> a -> Bool
isNotDifferentEnough a b = not $ 1 <= absDiff && absDiff <= 3
  where absDiff = abs $ b - a

carryOrdering :: (Ord a, Num a) => ReportSafety a -> a -> ReportSafety a
carryOrdering Unsafe _ = Unsafe
carryOrdering (Unknown x) y | isNotDifferentEnough x y = Unsafe
                            | y < x = Decreasing y
                            | y > x = Increasing y
                            | otherwise = Unsafe
carryOrdering (Increasing x) y | isNotDifferentEnough x y = Unsafe
                               | y > x = Increasing y
                               | otherwise = Unsafe
carryOrdering (Decreasing x) y | isNotDifferentEnough x y = Unsafe
                               | y < x = Decreasing y
                               | otherwise = Unsafe

reportSafety :: (Ord a, Num a) => [a] -> ReportSafety a
reportSafety (x:xs) = foldl carryOrdering (Unknown x) xs
reportSafety _ = Unsafe

isSafe :: ReportSafety a -> Bool
isSafe Unsafe = False
isSafe _ = True

howManyReportsSafe :: [ReportSafety a] -> Int
howManyReportsSafe = length . filter isSafe

-- brute force approach
isReportSafeWithProblemDampener :: (Ord a, Num a) => [a] -> Bool
isReportSafeWithProblemDampener r = any isSafe $ allPosibilities
  where removeNth _ [] = []
        removeNth 0 (_:xs) = xs
        removeNth n (x:xs) = x : removeNth (n-1) xs
        safetyWithNthRemoved nn rr = reportSafety $ removeNth nn rr
        replicated = replicate (length r) r
        allPosibilities = zipWith (flip safetyWithNthRemoved) replicated [0..] -- all the possible ways an element could be deleted

howManyReportsSafeWithProblemDampener :: (Ord a, Num a) => [[a]] -> Int
howManyReportsSafeWithProblemDampener = length . filter isReportSafeWithProblemDampener

main :: IO ()
main = do (inputPath : _) <- getArgs
          input <- getPuzzleInput inputPath
          print $ howManyReportsSafe $ map reportSafety input
          print $ howManyReportsSafeWithProblemDampener input
