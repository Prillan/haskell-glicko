module Ranking.Glicko.Inference ( predict
                                , boX
                                , fromBoX
                                , predictBoX
                                , BoX) where

import Ranking.Glicko.Core
import Ranking.Glicko.Types

import Data.Coerce (coerce)
import Statistics.Distribution
import Statistics.Distribution.Normal

predict :: Player -> Player -> Double
predict pla plb = cumulative dist (ra - rb)
  where Player { _rating = ra, _dev = da } = oldToNew pla
        Player { _rating = rb, _dev = db } = oldToNew plb
        dist = normalDistr 0 (1 + da + db)
-- TODO: Check the above ^

newtype BoX = BoX Integer
  deriving Show

boX :: Integer -> Maybe BoX
boX n = if odd n && 0 < n && n <= 11
           then Just $ BoX n
           else Nothing

fromBoX :: BoX -> Integer
fromBoX = coerce
{-# INLINE fromBoX #-}

predictBoX :: BoX -> Player -> Player -> Double
predictBoX n p1 p2 =
  sum $ map (\i -> fromInteger ((z + i) `choose` i) * p^w * q^i) [0..z]
  where p  = predict p1 p2
        q  = 1 - p
        w  = (n' + 1) `div` 2
        z  = w - 1
        n' = fromBoX n

choose :: Integer -> Integer -> Integer
n `choose` k
  | k > n     = 0
  | k' == 0   = 1
  | otherwise = p1 `div` p2
  where k' = min k (n - k)
        p1 = product . map (\i -> n - i) $ [0..k' - 1]
        p2 = product [1..k']
