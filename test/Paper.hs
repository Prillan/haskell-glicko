module Paper where

import Ranking.Glicko

players :: [Player]
players =
  [ Player { _pid = 1
           , _rating = 1500
           , _dev = 200
           , _vol = 0.06
           , _inactivity = 0
           , _age = 0 }

  , Player { _pid = 2
           , _rating = 1400
           , _dev = 30
           , _vol = 0.06
           , _inactivity = 0
           , _age = 0 }

  , Player { _pid = 3
           , _rating = 1550
           , _dev = 100
           , _vol = 0.06
           , _inactivity = 0
           , _age = 0 }

  , Player { _pid = 4
           , _rating = 1700
           , _dev = 300
           , _vol = 0.06
           , _inactivity = 0
           , _age = 0 }]

matches :: [Match]
matches =
  [ Match 1 2 1 0
  , Match 1 3 0 1
  , Match 1 4 0 1]
