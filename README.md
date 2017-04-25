# Glicko
[![Hackage](https://img.shields.io/hackage/v/glicko.svg)](https://hackage.haskell.org/package/glicko)
[![Dependencies Status](http://img.shields.io/hackage-deps/v/glicko.svg)](http://packdeps.haskellers.com/feed?needle=glicko)

Haskell implementation of the
[Glicko-2 rating algorithm](http://glicko.net/glicko/glicko2.pdf) by Professor Mark E. Glickman.

## Building

Easiest is using stack.

```
$ git clone https://github.com/Prillan/haskell-glicko.git
$ cd haskell-glicko
$ stack setup
$ stack build
```

## Examples

The test case from the Glicko-2 paper is included in `test/Paper.hs`.

```
$ stack ghci
*Main> :l test/Paper.hs
*Paper> :m + Data.Default
*Paper Data.Default> mapM_ print $ compute players matches def
Player {playerId = 1
  , playerRating = 1464.0506705393013
  , playerDev = 151.51652412385727
  , playerVol = 5.9995984286488495e-2
  , playerInactivity = 0
  , playerAge = 1}
Player {playerId = 2
  , playerRating = 1398.1435582337338
  , playerDev = 31.67021528115062
  , playerVol = 5.999912372888531e-2
  , playerInactivity = 0
  , playerAge = 1}
Player {playerId = 3
  , playerRating = 1570.394740240854
  , playerDev = 97.70916852200307
  , playerVol = 5.999941947199381e-2
  , playerInactivity = 0
  , playerAge = 1}
Player {playerId = 4
  , playerRating = 1784.4217901320874
  , playerDev = 251.56556453224735
  , playerVol = 5.9999011763670944e-2
  , playerInactivity = 0
  , playerAge = 1}
```
