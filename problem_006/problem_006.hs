#!/usr/bin/env runhaskell
-- project euler: problem 006
-- Keita Yamaguchi, 2010
-- Haskell version

module Main where

-- n^2 + (n+1)^2 + ...
sumOfSquares :: Integer -> Integer
sumOfSquares 0 = 0
sumOfSquares n = n^2 + sumOfSquares (n - 1)

-- (n + (n+1) + ...)^2
squareOfSum :: Integer -> Integer
squareOfSum n = (foldl (+) 0 [1..n])^2

-- main
main = print (res1 - res2)
  where
    n = 100
    res1 = squareOfSum n
    res2 = sumOfSquares n
