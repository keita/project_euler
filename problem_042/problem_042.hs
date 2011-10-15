import Data.Char
import IO

wordsFromText :: String -> [String]
wordsFromText txt =
  words $ foldl f "" txt
    where
      f s '"' = s
      f s ',' = s ++ " "
      f s c = s ++ [c]

numberOfChar :: Char -> Int
numberOfChar c = (ord c) - 64

numberOfWord word =
  foldl (\sum c -> sum + numberOfChar c) 0 word

triangleNumbers :: [Int]
triangleNumbers =
  triangleNumbers' 1 2

triangleNumbers' :: Int -> Int -> [Int]
triangleNumbers' a b =
  if n < 500 then (n:(triangleNumbers' (a+1) (b+1))) else [n]
  where
    n = div (a*b) 2

solve :: String -> Int
solve txt =
  length $ filter (\w -> elem (numberOfWord w) triangleNumbers) $ words
    where
      words = wordsFromText txt

main :: IO ()
main = do
  txt <- readFile "words.txt"
  putStrLn $ show $ solve txt