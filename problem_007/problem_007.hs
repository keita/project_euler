#!/usr/bin/env runhaskell
-- project euler: problem 007
-- Keita Yamaguchi, 2010
-- Haskell version

module Main where

isPrime :: Integer -> [Integer] -> Bool
isPrime _ [] = True
isPrime n (prime:primes) =
  if n `mod` prime == 0 then False else isPrime n primes

prime :: Integer -> [Integer] -> Integer
prime n primes =
  if length primes == 10001
  then head primes
  else
    if isPrime n primes then prime (n+1) (n:primes) else prime (n+1) primes

main = print (prime 2 [])