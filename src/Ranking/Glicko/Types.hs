{-# LANGUAGE TemplateHaskell #-}
module Ranking.Glicko.Types
       ( Player(..)
       , pid, rating, dev, vol, inactivity, age
       , Match(..)
       , pla, plb, sca, scb
       , PlayerId
       , Score
       , ScoreFunction(..)
       , GlickoSettings(..))
       where

import           Control.DeepSeq
import           Control.Lens
import           Data.Default

type PlayerId = Int
data Player = Player { _pid        :: PlayerId
                     , _rating     :: Double
                     , _dev        :: Double
                     , _vol        :: Double
                     , _inactivity :: Int
                     , _age        :: Int}
  deriving (Show, Eq)
makeLenses ''Player

instance NFData Player where
  rnf (Player x1 x2 x3 x4 x5 x6) = rnf (x1, x2, x3, x4, x5, x6)

type Score = Int
data Match = Match { _pla :: PlayerId
                   , _plb :: PlayerId
                   , _sca :: Score
                   , _scb :: Score}
  deriving (Show, Eq)
makeLenses ''Match

newtype ScoreFunction = ScoreFunction { compareScores :: Score -> Score -> Double }
instance Default ScoreFunction where
  def = ScoreFunction $ \s1 s2 -> case s1 `compare` s2 of
    LT -> 0
    EQ -> 0.5
    GT -> 1
instance Show ScoreFunction where
  show _ = "{score function}"

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
