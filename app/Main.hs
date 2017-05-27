module Main where

import HECC


main :: IO ()
main = do
  putStrLn "==Domain Parameters=="
  putStrLn "(p,a,b) = (5557,17,173)"
  putStr "G = "
  print g
  putStr "n = "
  print $ orderN 1 g
  putStrLn "==Key Generation=="
  putStrLn "(private key) d = 273"
  putStr "(public key) Q = dG = "
  print $ 273 × g
  putStrLn "==Signiture Generation=="
  putStrLn "k = 187"
  putStr "(r, _) = k G = "
  print $ 187 × g
  putStr "k^-1 = "
  print $ modInv 187 687
  putStrLn "e = 92185"
  putStr "s = k^-1 * (e + dr) = "
  print $ mod (169 * (92185 + 273*4385)) 687
  putStrLn "(signiture) = (r,s) = (4385,403)"
  putStrLn "==Signiture Verification=="
  putStr "w = s^-1 = "
  print $ modInv 403 687
  putStr "u1 = ew = "
  print $ mod (92185 * 433) 687
  putStr "u2 = rw = "
  print $ mod (4385 * 433) 687
  putStr "(x1,y1) = u1G + u2Q = "
  print $ 31 × g + 524 × (273 × g)
  putStrLn "r = v ✅"

isPrime::Integer -> (Bool,Integer)
isPrime n = if n < 2 then (False,n)
      else (null [x | x <- [2 .. n-1], n `mod` x == 0],n)
