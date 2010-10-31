#!/usr/bin/runhaskell
-- project euler: problem 2
-- Keita Yamaguchi, 2010
-- Haskell version

module Main(main) where

-- fibonacci sequence
fib max = f 1 []
  where
    f 1 ls = f 2 (1:ls)
    f 2 ls = f 3 (2:ls)
    f n ls =
      if r < max then f (n + 1) (r:ls) else ls
        where
          r = (head ls) + (head (tail ls))

evenseq = [x | x <- fib 4000000, even x]

main = print (sum evenseq)