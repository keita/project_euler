#!/usr/bin/env runhaskell
-- project euler: problem 005
-- Keita Yamaguchi, 2010
-- Haskell version

module Main where

-- is it divisible?
isDivisible i =
  f 11 && f 12 && f 13 && f 14 && f 15 &&
  f 16 && f 17 && f 18 && f 19 && f 20
  where
    f n = i `mod` n == 0

-- find divisible smallest number
findDivisible i =
  if isDivisible i then i else findDivisible (i+20)

-- main
main = print (findDivisible 20)