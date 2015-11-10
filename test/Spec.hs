import Data.Default
import Test.Hspec


import Paper
import Ranking.Glicko

epsilon = 0.0000001

doubleEqual :: Double -> Double -> Bool
doubleEqual x y = abs (x - y) <= epsilon

main :: IO ()
main = hspec $ do
  describe "Ranking.Glicko.Core" $ do
    it "Glicko2 paper test case (http://glicko.net/glicko/glicko2.pdf)" $ do
      let p:_ = compute players matches def
          d   = _dev p
          r   = _rating p
          v   = _vol p
      r `shouldSatisfy` (`doubleEqual` 1464.0506705393013)
      d `shouldSatisfy` (`doubleEqual` 151.51652412385727)
      v `shouldSatisfy` (`doubleEqual` 5.9995984286488495e-2)
