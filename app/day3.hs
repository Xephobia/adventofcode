import Text.Regex.TDFA ((=~),MatchText)
import Data.Array (elems)
import System.Environment (getArgs)
import Data.Functor ((<&>))

getPuzzleInput :: FilePath -> IO [String]
getPuzzleInput path = readFile path <&> lines

-- build a list of list of matchs muls. Each sublist is a list of argument of a mul
findAllMuls :: String -> [[Int]]
findAllMuls i = map extractArgs matchTexts
  where matchTexts = i =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" :: [MatchText String]
        extractArgs :: MatchText String -> [Int]
        extractArgs = map read . tail . elems . fmap fst -- convert the regex match array down to a list of ints
      
main :: IO ()
main = do (inputPath : _) <- getArgs
          input <- getPuzzleInput inputPath
          print $ sum $ map (sum . map product . findAllMuls) input
