module Main where

import Lib
import HECC

main :: IO ()
main = someFunc

basePoint :: (Int,Int)
basePoint = head $ filter (\l -> (snd l^2) `eqModP` (fst l^3 + 17*fst l + 173)) [(x,y)|x<-[0..5556],y<-[0..5556]]

orderN :: HECC5557 -> (Int,Int) -> Int
orderN Infinity _ = 0
orderN rho (x,y) = head $ filter (\n -> n × rho == Infinity ) [x..y]

origin = Point 0 2032
