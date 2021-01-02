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

    describe "day 4" $ do
      forM_ [("aa bb cc dd ee", True)
            ,("aa bb cc dd aa", False)
            ,("aa bb cc dd aaa", True)
            ] $ \(i,o) -> do
        it ("validates " ++ show i) $ do
          Day4.valid (words i) `shouldBe` o
      forM_ [("abcde fghij", True)
            ,("abcde xyz ecdab", False)
            ,("a ab abc abd abf abj", True)
            ,("iiii oiii ooii oooi oooo", True)
            ,("oiii ioii iioi iiio", False)
            ] $ \(i,o) -> do
        it ("extra-validates " ++ show i) $ do
          Day4.valid' (words i) `shouldBe` o

    describe "day 6" $ do
      forM_ [([0, 2, 7, 0], [2, 4, 1, 2])
            ,([2, 4, 1, 2], [3, 1, 2, 3])
            ,([3, 1, 2, 3], [0, 2, 3, 4])
            ,([0, 2, 3, 4], [1, 3, 4, 1])
            ,([1, 3, 4, 1], [2, 4, 1, 2])
            ] $ \(i,o) -> do
        it ("redistributes " ++ show i) $ do
          (i & Day6.l2m & Day6.redistribute & Day6.m2l) `shouldBe` o
    
      it "works out the size of the loop" $ do
        fst (Day6.shuffle 0 Set.empty (Day6.l2m [0, 2, 7, 0])) `shouldBe` 5

    describe "day 7" $ do
      let example = "pbga (66)\n\
                    \xhth (57)\n\
                    \ebii (61)\n\
                    \havc (66)\n\
                    \ktlj (57)\n\
                    \fwft (72) -> ktlj, cntj, xhth\n\
                    \qoyq (66)\n\
                    \padx (45) -> pbga, havc, qoyq\n\
                    \tknk (41) -> ugml, padx, fwft\n\
                    \jptl (61)\n\
                    \ugml (68) -> gyxo, ebii, jptl\n\
                    \gyxo (61)\n\
                    \cntj (57)" & lines
          stack = Day7.parse example
      it "locates the base" $ do
        Day7.base stack `shouldBe` "tknk"
