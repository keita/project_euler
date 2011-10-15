#!/usr/bin/env runhaskell
-- project euler: problem 009
-- Keita Yamaguchi, 2010
-- Haskell version

module Main where

-- is Pythagorean triplet?
isPythagoreanTriplet a b c =
  a < b && b < c && a^2 + b^2 == c^2

-- find Pythagorean
findPythagorean 1000 = 0
findPythagorean b c =
  if isPythagoreanTriplet a b c then res else findPythagorean (b+1) c
    where
      a = 1000 - b - c
      res = a * b * c
      f nb = if isPythagoreanTriplet a nb c

main = print (findPythagorean 1 998)