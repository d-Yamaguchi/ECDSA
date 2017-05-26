module Main where

import Lib
import HECC
import Control.Monad
import Control.Monad.State

main :: IO ()
main = someFunc

basePoint :: (Int,Int)
basePoint = head $ filter infCondition [(x,y)|x<-[0..5556],y<-[0..5556]]
  where
    eqMod5557 alpha beta = alpha `mod` 5557 == beta `mod` 5557
    infCondition l = (snd l^2) `eqMod5557` (fst l^3 + 17*fst l + 173)

orderN :: Int -> State HECC5557 Int
orderN n = do
  p <- get
  case p of
    Infinity -> return n
    Point x y -> put (p + g) >> orderN (succ n)

g = Point 0 2032

--n G = Infinity
n = 500

modDiv alpha beta = (alpha * (beta ^ (n-2 :: Int))) `mod` n
eqModP alpha beta = alpha `mod` n == beta `mod` n
