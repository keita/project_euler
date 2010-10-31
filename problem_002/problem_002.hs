#!/usr/bin/runhaskell
-- project euler: problem 2
-- Keita Yamaguchi, 2010
-- Haskell version

module Main(main) where

-- fibonacci sequence
fib max 1 ls = fib max 2 (1:ls)
fib max 2 ls = fib max 3 (2:ls)
fib max n ls =
  if r < max then fib max (n + 1) (r:ls) else ls
  where
    r = r1 + r2
    r1 = head ls
    r2 = head (tail ls)

evenseq = [x| x <- fib 4000000 1 [], even x]

main = print (sum evenseq)