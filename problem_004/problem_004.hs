#!/usr/bin/env runhaskell
-- project euler: problem 4
-- Keita Yamaguchi, 2010
-- Haskell version

module Main (main) where

-- Is a value palindromic?
isPalindromic n = reverse (show n) == show n

-- generate palindrome numbers and return greater one
palindrome :: Int -> Int -> Int -> Int -> Int
palindrome min max n1 n2
  | n1 > n2 = f min (n2 + 1) -- have calced at reverse pair: 123*122 == 122*123
  | n2 >= max = 0            -- stop if reached at max
  | n1 >= max = getResult (f min (n2 + 1)) -- next n2
  | otherwise = getResult (f (n1 + 1) n2)  -- next n1
    where
      n = n1 * n2
      f = palindrome min max
      getResult rec = if isPalindromic n && n > rec then n else rec

-- get max palindrome and print it
main =
  print (palindrome min max min min)
    where
      min = 100
      max = 1000
