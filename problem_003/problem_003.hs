#!/usr/bin/runhaskell
-- project euler: problem 3
-- Keita Yamaguchi, 2010
-- Haskell version

module Main where

primeFactorsOf :: Integer -> Integer -> [Integer]
primeFactorsOf 1 _ = []
primeFactorsOf n i =
  if m == 0 then (i:nextPrimes) else tryNextNumber
    where
      (d, m) = divMod n i
      nextPrimes = primeFactorsOf d 2
      tryNextNumber = primeFactorsOf n (i+1)

main = print (primeFactorsOf 600851475143 2)
