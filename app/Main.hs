module Main where

import Lib
import HECC
import Control.Monad
import Control.Monad.State

main :: IO ()
main = someFunc

basePoints :: [(Int,Int)]
basePoints = take 10 $ filter infCondition [(x,y)|x<-[1..p],y<-[1..p]]
  where
    eqMod5557 alpha beta = alpha `mod` p == beta `mod` p
    infCondition l = (snd l^2) `eqMod5557` (fst l^3 + 17*fst l + 173)
    p = 5557

orderN :: Int -> HECC5557 -> Int
orderN n Infinity = n
orderN n p
     | n == 5000 = -1
     | otherwise = orderN (n + 1) (p + g)

g = undefined

gSet :: (Int, Int) -> HECC5557
gSet = uncurry Point

--n G = Infinity
n = 500

modDiv alpha beta = (alpha * (beta ^ (n-2 :: Int))) `mod` n
eqModP alpha beta = alpha `mod` n == beta `mod` n
