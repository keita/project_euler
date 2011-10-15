
makeB10 :: Int -> Maybe Int -> Int
makeB10 i Nothing =
  read $ show i ++ reverse (show i)
makeB10 i (Just j) =
  read $ show i ++ show j ++ reverse (show i)

genPalindromicB10 :: Int -> [Int]
genPalindromicB10 1 = [1..9]
genPalindromicB10 2 = genPalindromicB10' [1..9] False
genPalindromicB10 3 = genPalindromicB10' [1..9] True
genPalindromicB10 4 = genPalindromicB10' [10..99] False
genPalindromicB10 5 = genPalindromicB10' [10..99] True
genPalindromicB10 6 = genPalindromicB10' [100..999] False

genPalindromicB10' :: [Int] -> Bool -> [Int]
genPalindromicB10' li False =
  map (\i -> makeB10 i Nothing) li
genPalindromicB10' li True =
  map (\(i,j) -> makeB10 i (Just j)) pairs
    where
      pairs = foldl (\l i -> l ++ map (\j -> (i,j)) [0..9]) [] li

palindromicB10 :: [Int]
palindromicB10 =
  foldl (\l i -> l ++ genPalindromicB10 i) [] [1..6]

convB10ToB2 :: Int -> String
convB10ToB2 0 = "0"
convB10ToB2 1 = "1"
convB10ToB2 n =
  convB10ToB2 i ++ convB10ToB2 j
    where
      (i,j) = divMod n 2

isPalindromic :: String -> Bool
isPalindromic s = s == reverse s

solve :: Int
solve =
  foldl (\sum i -> if isPalindromic (convB10ToB2 i)
                   then sum + i
                   else sum) 0 palindromicB10

main :: IO ()
main = putStrLn $ show solve