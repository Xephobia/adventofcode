module Common where

getPuzzleInput :: FilePath -> IO ([Int],[Int])
getPuzzleInput path = do raw <- readFile path
                         return $ unzip $ map (tuplify2 . (map read . words)) $ lines raw
                           where tuplify2 [x,y] = (x,y)
