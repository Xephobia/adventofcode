import Data.Graph (Vertex, graphFromEdges)
import System.Environment (getArgs)

getPuzzleInput :: FilePath -> IO [[Char]]
getPuzzleInput path = do
  raw <- readFile path
  return $ lines raw

type Node = Char

type Key = (Int, Int)

pairwiseSum :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
pairwiseSum (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

adjacentKeys :: (Num a, Num b) => (a, b) -> [(a, b)]
adjacentKeys xy = map (pairwiseSum xy) [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

-- build a graph of the letter input. Each letter is linked to the adjacent letters, even if they don't exist (negative key)
buildLetterGraph :: [[Char]] -> (Vertex -> (Node, Key, [Key]), Key -> Maybe Vertex)
buildLetterGraph css = (nodeFromVertex, vertexFromKey)
  where
    edgeList :: [(Node, Key, [Key])]
    edgeList = concat $ zipWith (\cs y -> zipWith (\c x -> (c, (x, y), adjacentKeys (x, y))) cs [0 ..]) css [0 ..]
    (_, nodeFromVertex, vertexFromKey) = graphFromEdges edgeList

-- get all the letters in the chosen direction d from position p
getNLettersInDirection :: (Vertex -> (Node, Key, [Key])) -> (Key -> Maybe Vertex) -> Int -> Key -> Key -> [Char]
getNLettersInDirection nfv vfk n p d = go n p []
  where
    go :: Int -> Key -> [Char] -> [Char]
    go 0 _ ls = reverse ls
    go nn pp ls = case node of
      Just (c, _, _) -> go (nn - 1) nextPosition (c : ls)
      Nothing -> reverse ls
      where
        nextPosition = pairwiseSum pp d
        node = nfv <$> vfk nextPosition

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  input <- getPuzzleInput inputPath
  let (nfv, vfk) = buildLetterGraph input
  let lettersList = concat $ zipWith (\cs y -> zipWith (\c x -> (c, (x, y))) cs [0 ..]) input [0 ..]
  let directions = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]
  print $ length $ concatMap (\d -> filter (== "XMAS") $ map (\(c, xy) -> c : getNLettersInDirection nfv vfk 3 xy d) $ filter (('X' ==) . fst) lettersList) directions
  let getX p = zipWith (getNLettersInDirection nfv vfk 3 . pairwiseSum p) [(-2, -2), (2, -2)] [(1, 1), (-1, 1)]
  print $ length $ filter (\(_, p) -> all (\s -> (s == "MAS") || (reverse s == "MAS")) $ getX p) $ filter ((== 'A') . fst) lettersList
