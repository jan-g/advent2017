import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad
import Data.List.Split (splitOn)
import Data.List as L
import Data.Maybe (catMaybes, fromJust)
import qualified Data.ByteString.UTF8 as BSU

import Lib
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25


main :: IO ()
main =
  hspec $ do
    describe "Day1" $ do
      it "correctly runs on the test data" $ do
        Day1.day1 ["91212129"] `shouldBe` 9
      it "does part 2" $ do
        Day1.day1b ["12131415"] `shouldBe` 4

    describe "day 3" $ do
      forM_ [(1,1), (2,2), (9, 2), (10,3), (25,3), (26,4)] $ \(i,o) -> do
        it ("computes rings correctly for " ++ show i) $ do
          Day3.ringOf i `shouldBe` o
      forM_ [(1,0), (2,0), (9, 7), (10,0), (25,15), (26,0)] $ \(i,o) -> do
        it ("computes circuits correctly for " ++ show i) $ do
          Day3.circuit i `shouldBe` o
      forM_ [(1,1), (2,3), (9, 3), (10,5), (25,5), (26,7)] $ \(i,o) -> do
        it ("computes side lengths correctly for " ++ show i) $ do
          Day3.sideLength i `shouldBe` o
      forM_ [(1,(0, 0)), (2,(1, 0)), (3, (1,1)), (4, (0,1)), (5, (-1,1)), (6, (-1,0)), (7, (-1,-1)), (8, (0, -1)), (9, (1,-1)),
             (10,(2,-1)), (12, (2, 1)), (13, (2,2)), (14, (1,2)), (16, (-1,2)), (17, (-2,2)), (18, (-2,1)), (20, (-2,-1)), (21, (-2,-2)), (22, (-1,-2)), (25,(2,-2)),
             (26,(3,-2))] $ \(i,o) -> do
        it ("computes positiong for " ++ show i) $ do
          Day3.position i `shouldBe` o

      forM_ [(1,0), (12,3), (23, 2), (1024,31)] $ \(i,o) -> do
        it ("computes example manhattan distance for " ++ show i) $ do
          Day3.manh (Day3.position i) `shouldBe` o
