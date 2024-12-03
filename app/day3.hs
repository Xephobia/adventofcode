import Data.Array (elems)
import Data.Functor ((<&>))
import Data.List (group)
import System.Environment (getArgs)
import Text.Regex.TDFA (MatchText, (=~))

getPuzzleInput :: FilePath -> IO String
getPuzzleInput path = readFile path <&> (concat . lines)

data InstructionType = Mul Int Int | Do | Dont
  deriving (Show)

instance Eq InstructionType where
  (Mul _ _) == (Mul _ _) = True
  Do == Do = True
  Dont == Dont = True
  _ == _ = False

executeMul :: InstructionType -> Int
executeMul (Mul a b) = a * b
executeMul _ = 0

-- build a list of list of matchs muls. Each sublist is a list of argument of a mul
findAllMuls :: String -> [InstructionType]
findAllMuls i = map extractArgs matchTexts
  where
    matchTexts = i =~ "do(n't)?\\(\\)|mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" :: [MatchText String]
    extractArgs :: MatchText String -> InstructionType
    extractArgs match = case map fst $ filter ((0 <=) . (fst . snd)) $ elems match of
      ("do()" : _) -> Do
      ("don't()" : _) -> Dont
      (_ : a1 : a2 : _) -> Mul (read a1) (read a2)

discardInstructions :: [[InstructionType]] -> [InstructionType]
discardInstructions ([Do] : r) = handleAdjacency r
  where
    handleAdjacency ([Dont] : _ : rs) = discardInstructions rs
    handleAdjacency ([Do] : is) = handleAdjacency is
    handleAdjacency (is : rs) = is ++ discardInstructions rs
discardInstructions ([Dont] : _ : r) = discardInstructions r
discardInstructions [] = []

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  input <- getPuzzleInput inputPath
  print $ sum $ map executeMul $ findAllMuls input
  print $ sum $ map executeMul $ discardInstructions $ ([Do] :) . group $ findAllMuls input
