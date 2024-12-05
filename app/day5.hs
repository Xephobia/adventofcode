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

main :: IO ()
main = do
  (inputPath : _) <- getArgs
  (os, us) <- getPuzzleInput inputPath
  let getMiddle l = l !! (length l `div` 2)
  print $ sum $ map getMiddle $ filter (respectsAll os) us
