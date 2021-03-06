{-|
Module      : Ranking.Glicko.Types
License     : GPL-3
Maintainer  : rasmus@precenth.eu
Stability   : experimental

For examples, see `Ranking.Glicko.Core` and `Ranking.Glicko.Inference`.
-}
{-# LANGUAGE KindSignatures #-}
module Ranking.Glicko.Types
       ( -- * Data types
         Player(..)
       , Match(..)
       , PlayerId
       , Score
       , ScoreFunction(..)
       , GlickoSettings(..) )
       where

import           Control.DeepSeq
import           Data.Default
import           GHC.TypeLits (Nat)

type PlayerId = Int
-- | Data type representing a player's Glicko rating. The type
--  'version' is used to differentiate between Glicko ('Player' 1) and
--  Glicko-2 ('Player' 2).
data Player (version :: Nat) =
  Player { playerId         :: PlayerId -- ^ Player id, can be anything
         , playerRating     :: Double   -- ^ Rating
         , playerDev        :: Double   -- ^ Deviation
         , playerVol        :: Double   -- ^ Volatility
         , playerInactivity :: Int      -- ^ Inactivity (not part of Glicko-2),
                                        -- keeps track of the number of rating
                                        -- updates a player has been inactive.
         , playerAge        :: Int      -- ^ Age (not part of Glicko-2),
                                        -- keeps track of the number of rating
                                        -- updates since the player was added.
                     }
  deriving (Show, Eq)

instance NFData (Player v) where
  rnf (Player x1 x2 x3 x4 x5 x6) = rnf (x1, x2, x3, x4, x5, x6)

type Score = Int
data Match = Match { matchPlayerA :: PlayerId
                   , matchPlayerB :: PlayerId
                   , matchScoreA :: Score
                   , matchScoreB :: Score}
  deriving (Show, Eq)

-- | 'ScoreFunction's are used in 'compute' to evaluate two players performances against
-- eachother. It should obey the following laws,
--
--
-- prop>  0 <= compareScores x y
-- prop>  1 >= compareScores x y
-- prop>  compareScores x y == 1 - compareScores y x
--
--
-- The default implementation is
--
-- @
-- \\s1 s2 -> case s1 \`compare\` s2 of
--             LT -> 0
--             EQ -> 0.5
--             GT -> 1
-- @
newtype ScoreFunction = ScoreFunction { compareScores :: Score -> Score -> Double }
instance Default ScoreFunction where
  def = ScoreFunction $ \s1 s2 -> case s1 `compare` s2 of
    LT -> 0
    EQ -> 0.5
    GT -> 1
instance Show ScoreFunction where
  show _ = "{score function}"

-- | Provides the 'Ranking.Glicko.Core.compute' function with parameters.
-- See <http://glicko.net/glicko/glicko2.pdf> for an explanation.
--
-- (NOTE: 'scoreFunction' is not a part of Glicko-2)
--
-- The default settings are as defined in the above paper.
data GlickoSettings = GlickoSettings
  { initialRating     :: Double
  , initialDeviation  :: Double
  , initialVolatility :: Double
  , tau               :: Double
  , scoreFunction     :: ScoreFunction}
  deriving Show

instance Default GlickoSettings where
  def = GlickoSettings
      { initialRating     = 1500
      , initialDeviation  = 350
      , initialVolatility = 0.06
      , tau = 0.5
      , scoreFunction = def}
