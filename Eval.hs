module Eval where
import Grammar
import Data.List

type Table = [(String, Int, [String])]

eval1 :: Statement -> IO Table
eval1 (CSVStatement x) = processFile x

processFile :: CSVFile -> IO Table
processFile (File path) = do
  sourceText <- readFile path
  let name = take (length path - 4) path
  let cols = splitCols sourceText
  return ([(name, fst x, snd x) | x <- zip [0..] cols])

splitCols :: String -> [[String]]
splitCols xs = transpose (map (splitOn ',') (splitOn '\n' xs))

splitOn :: Char -> String -> [String]
splitOn c [] = [""]
splitOn c (x:[]) | x == c = [[]]
splitOn c (x:xs) | x == c = "":(splitOn c xs)
splitOn c (x:xs) | otherwise = (x : head (splitOn c xs)):(tail $ splitOn c xs)
