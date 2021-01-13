module Paper where

import Ranking.Glicko

players :: [Player 1]
players =
  [ Player { playerId = 1
           , playerRating = 1500
           , playerDev = 200
           , playerVol = 0.06
           , playerInactivity = 0
           , playerAge = 0 }

  , Player { playerId = 2
           , playerRating = 1400
           , playerDev = 30
           , playerVol = 0.06
           , playerInactivity = 0
           , playerAge = 0 }

  , Player { playerId = 3
           , playerRating = 1550
           , playerDev = 100
           , playerVol = 0.06
           , playerInactivity = 0
           , playerAge = 0 }

  , Player { playerId = 4
           , playerRating = 1700
           , playerDev = 300
           , playerVol = 0.06
           , playerInactivity = 0
           , playerAge = 0 }]

matches :: [Match]
matches =
  [ Match 1 2 1 0
  , Match 1 3 0 1
  , Match 1 4 0 1]
