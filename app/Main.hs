module Main where

import Lib
import HECC

main :: IO ()
main = someFunc

basePoint :: (Int,Int)
basePoint = head $ filter (\l -> (snd l^2) `eqMod5557` (fst l^3 + 17*fst l + 173)) [(x,y)|x<-[0..5556],y<-[0..5556]]
  where
    eqMod5557 alpha beta = alpha `mod` 5557 == beta `mod` 5557

orderN :: HECC5557 -> (Int,Int) -> Int
orderN Infinity _ = 0
orderN rho (x,y) = head $ filter (\n -> n × rho == Infinity ) [x..y]

--point G
g = Point 0 2032

--n G = Infinity
n = 500

modDiv alpha beta = (alpha * (beta ^ (n-2 :: Int))) `mod` n
eqModP alpha beta = alpha `mod` n == beta `mod` n
