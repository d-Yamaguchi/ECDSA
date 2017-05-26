module Lib where

import HECC

someFunc :: IO ()
someFunc = putStrLn "someFunc"

printLambdaNu :: HECC5557 -> HECC5557 -> IO()
printLambdaNu (Point x1 y1) (Point x2 y2) = print (lambda,nu)
  where
        lambda | eqMod17 x1 x2 = (3 * x1^ (2::Int) + 2 * a2 * x1 + a4 -a1 * y1) `modDiv` (2 * y1 + a1 * x1 + a3)
          | otherwise = (y2 - y1) `modDiv` (x2 - x1)
        nu | eqMod17 x1 x2  = ((-x1)^ (3::Int) + a4*x1 + 2*a6 - a3*y1) `modDiv` (2*y1 + a1*x1 + a3)
          | otherwise = (y1 * x2 - y2 * x1) `modDiv` (x2 - x1)
        (a1,a2,a3,a4,a6) = (0,0,0,2,2)
        modDiv alpha beta = head $ filter (\gamma -> alpha `eqMod17` (gamma * beta)) [1..17]
        eqMod17 alpha beta = alpha `mod` 17 == beta `mod` 17
printLambdaNu _ _ = print Infinity


isPrime::Integer -> (Bool,Integer)
isPrime n = if n < 2 then (False,n)
    else (null [x | x <- [2 .. n-1], n `mod` x == 0],n)
