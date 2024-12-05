import Data.List (groupBy, sortBy)
import qualified Data.Map as Map
import System.Environment (getArgs)

type Page = Int

type PageOrdering = (Page, Page)

type Update = [Page]

tuplify2 :: [a] -> (a, a)
tuplify2 (x : y : _) = (x, y)

wordsWhen :: Char -> String -> [String]
wordsWhen c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : wordsWhen c s''
    where
      (w, s'') = break (== c) s'

getPuzzleInput :: FilePath -> IO ([PageOrdering], [Update])
getPuzzleInput path = do
  allData <- readFile path
  let (ordersS, _ : updatesS) = break (== "") $ lines allData
  let orders = map (tuplify2 . map read . wordsWhen '|') ordersS
  let updates = map (map read . wordsWhen ',') updatesS
  return (orders, updates)

respectsAll :: [PageOrdering] -> Update -> Bool
respectsAll os u = all respects os
  where
    en = zip u [0 ..]
    respects (b, a) = case (lookup b en, lookup a en) of
      (Just b', Just a') -> b' < a'
      _ -> True

buildPredecessors :: [PageOrdering] -> Map.Map Page [Page]
buildPredecessors os = Map.fromList predsList
  where
    updatePred (x, p) (_, p') = (x, p ++ p')
    equalFst (x1, _) (x2, _) = x1 == x2
    cmpFst (x1, _) (x2, _) = x1 `compare` x2
    makePred (x, y) = (y, [x])
    predsList = map (foldl1 updatePred) $ groupBy equalFst $ sortBy cmpFst $ map makePred os

specialOrd :: Map.Map Page [Page] -> Page -> Page -> Ordering
specialOrd ps a b = case (bInA, aInB) of
  (True, False) -> GT
  (False, True) -> LT
  _ -> EQ
  where
    bInA = maybe False (elem b) $ Map.lookup a ps
    aInB = maybe False (elem a) $ Map.lookup b ps

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  (os, us) <- getPuzzleInput inputPath
  let trueOrdered = buildPredecessors os
  let getMiddle l = l !! (length l `div` 2)
  print $ sum $ map getMiddle $ filter (respectsAll os) us
  print $ sum $ map (getMiddle . sortBy (specialOrd trueOrdered)) $ filter (not . respectsAll os) us
