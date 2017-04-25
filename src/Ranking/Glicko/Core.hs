{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Ranking.Glicko.Core
License     : GPL-3
Maintainer  : prillan91@gmail.com
Stability   : experimental

This module contains the main function, 'compute'. Use this to compute new ratings from
old ones.

>>> let ps = compute [] [Match 1 2 1 0] def
>>> ps
[ Player { playerId = 1
         , playerRating = 1662.3108939062977
         , playerDev = 290.31896371798047
         , playerVol = 5.999967537233814e-2
         , playerInactivity = 0
         , playerAge = 1 }
, Player { playerId = 2
         , playerRating = 1337.6891060937023
         , playerDev = 290.31896371798047
         , playerVol = 5.999967537233814e-2
         , playerInactivity = 0
         , playerAge = 1 }]
>>> compute ps [Match 1 3 0 0] def
[ Player { playerId = 1
         , playerRating = 1623.996484575735
         , playerDev = 256.3451684359266
         , playerVol = 5.999869083062934e-2
         , playerInactivity = 0
         , playerAge = 2 }
, Player { playerId = 2
         , playerRating = 1337.6891060937023
         , playerDev = 290.5060065906196
         , playerVol = 5.999967537233814e-2
         , playerInactivity = 1
         , playerAge = 2 }
, Player { playerId = 3
         , playerRating = 1557.6214863132009
         , playerDev = 286.9272058793522
         , playerVol = 5.999899836136578e-2
         , playerInactivity = 0
         , playerAge = 1 }]
-}
module Ranking.Glicko.Core
       ( compute
       , computeP
       , newToOld
       , oldToNew ) where

import Prelude hiding ((^))
import qualified Prelude as P

import Data.Maybe
import Control.Parallel.Strategies
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Ranking.Glicko.Types

(^) :: Double -> Integer -> Double
(^) = (P.^)

-- Run map in parallel
pMap :: NFData b => Int -> (a -> b) -> [a] -> [b]
pMap chunkSize f = withStrategy (parListChunk chunkSize rdeepseq) . map f

-- | Computes new ratings from the previous and adds new ones using the
-- specified settings.
compute :: [Player]       -- ^ Input players
        -> [Match]        -- ^ Matches played this period
        -> GlickoSettings -- ^ Settings for computing the score values and adding new
                          -- players.
        -> [Player]       -- ^ Updated player ratings
compute = compute' map

-- | Same as 'compute' but runs in parallel using the specified chunkSize
computeP :: Int -> [Player] -> [Match] -> GlickoSettings -> [Player]
computeP chunkSize = compute' (pMap chunkSize)

-- Update all player ratings
compute' :: (((PlayerId, Player) -> Player) -> [(PlayerId, Player)] -> [Player])
         -> [Player]
         -> [Match]
         -> GlickoSettings
         -> [Player]
compute' map' ps ms settings = map' (newToOld . updater . snd) . Map.toList $ pmap'
  where pmap = Map.fromList $ map (\p -> (playerId p, p)) ps
        pmap' = preprocess pmap ms settings
        matches = preprocessMatches pmap' ms
        updater p = updatePlayer p matches settings

-- Compute new rating for player
updatePlayer :: Player -> [RatedMatch] -> GlickoSettings -> Player
updatePlayer p ms GlickoSettings{ tau = tau, scoreFunction = scoreFun }
  | null matches = p { playerDev = sqrt (pφ^2 + pσ^2)
                     , playerInactivity = playerInactivity p + 1
                     , playerAge        = playerAge p + 1 }
  | otherwise    = p { playerDev        = φ'
                     , playerRating     = µ'
                     , playerVol        = σ'
                     , playerInactivity = 0
                     , playerAge        = playerAge p + 1 }
  where -- Initial values for player
        pµ = playerRating p
        pφ = playerDev p
        pσ = playerVol p
        -- Values for opponent in match `m`
        µ (_, opp, _, _) = playerRating opp
        φ (_, opp, _, _) = playerDev opp
        -- Score value for match
        s :: RatedMatch -> Double
        s (_,_,sa,sb) = compareScores scoreFun sa sb
        -- Convenience function for E(µ, µj, φj)
        e m = _E pµ (µ m) (φ m)
        -- Step 3: v
        v = 1 / summer (\m -> _g (φ m)^2 * e m * (1 - e m))
        -- Step 4: ∆
        delta = v * step4sum
        -- Step 5: σ'
        σ' = calcSigma delta pφ pσ v tau
        -- Step 6: φ∗
        φstar = sqrt (pφ^2 + σ'^2)
        -- Step 7: φ' and µ'
        φ' = 1 / sqrt (1 / φstar^2 + 1 / v)
        µ' = pµ + φ'^2 * step4sum

        -- Helper used in both ∆ and µ'
        step4sum = summer (\m -> _g (φ m) * (s m - e m))
        -- Helper to abstract `Sum from j=1 to m`
        summer :: (RatedMatch -> Double) -> Double
        summer f = sum . map f $ matches

        -- All matches `p` played in, arranged so that `p` is the first player
        matches :: [RatedMatch]
        matches = map swap . filter (\(pla, plb, _, _) -> pla == p || plb == p) $ ms

        swap :: RatedMatch -> RatedMatch
        swap m@(pla, plb, sca, scb)
         | pla == p  = m
         | otherwise = (plb, pla, scb, sca)

type RatedMatch = (Player, Player, Score, Score)

-- g and E from step 3-4
_g :: Double -> Double
_g φ = 1 / sqrt (1 + 3*φ^2/(pi^2))

_E :: Double -> Double -> Double -> Double
_E µ µj φj = 1 / (1 + exp (- _g φj * (µ - µj)))

-- Computes σ' in step 5
calcSigma :: Double -> Double -> Double -> Double -> Double -> Double
calcSigma delta φ σ v tau = step a b (f a) (f b)
  where step a' b' fa fb
          | abs (b' - a') <= ε = exp (a'/2)
          | fc*fb         < 0  = step b' c fb     fc
          | otherwise          = step a' c (fa/2) fc
          where c = a' + (a' - b') * fa/(fb - fa)
                fc = f c
        a = log $ σ ^ 2
        b = if delta^2 > φ^2 + v then log (delta^2 - φ^2 - v) else fixB 1
        fixB k = if f (a - k*tau) < 0 then fixB (k+1) else a - k*tau
        f x = (exp x * (delta^2 - φ^2 - v - exp x)) / (2 * (φ^2 + v + exp x)^2) - (x - a) / tau^2

-- Tolerance used in calcSigma
ε :: Double
ε = 0.000001

-- Add new default players where missing
preprocess :: Map PlayerId Player -> [Match] -> GlickoSettings -> Map PlayerId Player
preprocess ps ms settings =
  Map.map oldToNew
  . Map.union ps
  . Map.fromList
  . map (\i -> (i, defaultPlayer { playerId = i }))
  . Set.toList $ diff
  where playersInMatches = Set.fromList $ (\m -> [matchPlayerA m, matchPlayerB m]) =<< ms
        players = Map.keysSet ps
        diff = Set.difference playersInMatches players
        defaultPlayer = Player { playerId = -1
                               , playerRating = initialRating settings
                               , playerDev = initialDeviation settings
                               , playerVol = initialVolatility settings
                               , playerInactivity = 0
                               , playerAge = 0}


-- Pull the players into the matches
preprocessMatches :: Map PlayerId Player -> [Match] -> [RatedMatch]
preprocessMatches ps = mapMaybe (
    \m -> (,,,)
      <$> Map.lookup (matchPlayerA m) ps
      <*> Map.lookup (matchPlayerB m) ps
      <*> pure (matchScoreA m)
      <*> pure (matchScoreB m)
  )

-- | Convert ratings from Glicko to Glicko-2
oldToNew :: Player -> Player
oldToNew p@Player{ playerRating = r, playerDev = d} = p { playerRating = (r - 1500) / glicko2Multiplier
                                              , playerDev    = d / glicko2Multiplier }

-- | Convert ratings from Glicko-2 to Glicko
newToOld :: Player -> Player
newToOld p@Player{ playerRating = r, playerDev = d} = p { playerRating = r*glicko2Multiplier + 1500
                                              , playerDev    = d*glicko2Multiplier}

glicko2Multiplier :: Double
glicko2Multiplier = 173.7178

playersToMap :: [Player] -> Map PlayerId Player
playersToMap = Map.fromList . map (\p -> (playerId p, p))
