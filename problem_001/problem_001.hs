#!/usr/bin/runhaskell
-- project euler: problem 1
-- Keita Yamaguchi, 2010
-- Haskell version

module Main(main) where
import List

mul n = [x | x <- [1..1000], (x `mod` n) == 0]

main = print res
  where
  res = sum ((mul 3) `union` (mul 5))
