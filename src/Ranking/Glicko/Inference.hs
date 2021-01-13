{-|
Module      : Ranking.Glicko.Inference
License     : GPL-3
Maintainer  : rasmus@precenth.eu
Stability   : experimental

This module provides functions for predicting the outcome of a game between two players.

Example usage:

>>> :l test/Paper.hs
>>> :m + Data.Default
>>> let p1:p2:_ = compute players matches def
>>> p1
Player { playerId = 1
       , playerRating = 1464.0506705393013
       , playerDev = 151.51652412385727
       , playerVol = 5.9995984286488495e-2
       , playerInactivity = 0
       , playerAge = 1 }
>>> p2
Player { playerId = 2
       , playerRating = 1398.1435582337338
       , playerDev = 31.67021528115062
       , playerVol = 5.999912372888531e-2
       , playerInactivity = 0
       , playerAge = 1 }
>>> predict p1 p2
0.5732533698644847     -- Player 1 has a 57.3% chance of winning a single game.
>>> let Just f = boX 5
>>> predictBoX f p1 p2
0.6353973157904573     -- Player 1 has a 63.5% chance of winning a best-of-five match.

-}
module Ranking.Glicko.Inference ( predict
                                , predictBoX
                                , BoX
                                , boX
                                , fromBoX) where

import Ranking.Glicko.Core
import Ranking.Glicko.Types

import Data.Coerce (coerce)
import Statistics.Distribution
import Statistics.Distribution.Normal

-- | Computes the probability that Player A wins against Player B
predict :: Player 1 -- ^ Player A
        -> Player 1 -- ^ Player B
        -> Double
predict pla plb = cumulative dist (ra - rb)
  where Player { playerRating = ra, playerDev = da } = oldToNew pla
        Player { playerRating = rb, playerDev = db } = oldToNew plb
        dist = normalDistr 0 (1 + da + db)
-- TODO: Check the above ^

-- | Represents a match played as best-of-X games.
newtype BoX = BoX Integer
  deriving Show

-- | Create a best-of-X match
boX :: Integer -> Maybe BoX
boX n = if odd n && 0 < n && n <= 11
           then Just $ BoX n
           else Nothing

-- | Destruct a best-of-X match
fromBoX :: BoX -> Integer
fromBoX = coerce
{-# INLINE fromBoX #-}

-- | Same as 'predict', but computes the probability that
-- Player A wins a match played as best-of-X games.
predictBoX :: BoX -> Player 1 -> Player 1 -> Double
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
