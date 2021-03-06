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
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

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

    describe "day 8" $ do
      let example = "b inc 5 if a > 1\n\
                    \a inc 1 if b < 5\n\
                    \c dec -10 if a >= 1\n\
                    \c inc -20 if c == 10" & lines
          prog = Day8.parse example
      it "runs the example" $ do
        let regs = Day8.run prog Map.empty
        map (Day8.fetch regs) ["a", "b", "c" ] `shouldBe` [1, 0, -10]

    describe "day 9" $ do
      forM_ [("{}", 1)
            ,("{{{}}}", 6)
            ,("{{},{}}", 5)
            ,("{{{},{},{{}}}}", 16)
            ,("{<a>,<a>,<a>,<a>}", 1)
            ,("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9)
            ,("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9)
            ,("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)
            ] $ \(i, o) -> do
        it ("evaluates " ++ i) $ do
          quickParse (Day9.grp 1 <* P.eof) i `shouldBe` Just o

    describe "day 10" $ do
      let r0 = Day10.ring 5
      it "does the example" $ do
        let r1 = Day10.step 0 r0 3
        toList r1 `shouldBe` [3, 4, 2, 1, 0]
        let r2 = Day10.step 1 r1 4
        toList r2 `shouldBe` [1, 2, 4, 3, 0]
        let r3 = Day10.step 2 r2 1
        toList r3 `shouldBe` [3, 0, 1, 2, 4]
--        let r4 = Day10.step 3 r3 5
--        toList r4 `shouldBe` [0, 3, 4, 2, 1]

--        toList (Day10.rotate (-(0 + 1 + 2 + 3 + 3 + 4 + 1 + 5)) r4) `shouldBe` [3, 4, 2, 1, 0]

      forM_ [("", "a2582a3a0e66e6e86e3812dcb672a272")
            ,("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd")
            ,("1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d")
            ,("1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e")
            ] $ \(i, o) -> do
        it ("evaluates " ++ i) $ do
          Day10.day10b [i] `shouldBe` o

    describe "day 11" $ do
      forM_ [ ("ne,ne,ne", (3,0,-3), 3)
            , ("ne,ne,sw,sw", (0,0,0), 0)
            , ("ne,ne,s,s", (2,-2,0), 2)
            , ("se,sw,se,sw,sw", (-1,-2,3), 3)
            ] $ \(i,o,d) -> do
        it ("follows the path " ++ i) $ do
          let pos = Day11.follow (0,0,0) (Day11.parse [i])
          pos `shouldBe` o
          Day11.manh pos `shouldBe` d

    describe "day 13" $ do
      let example = "0: 3\n\
                    \1: 2\n\
                    \4: 4\n\
                    \6: 4" & lines
      it "works out the example for part a" $ do
        Day13.day13 example `shouldBe` 24
      it "works out the answer for part b" $ do
        Day13.day13b example `shouldBe` 10

    describe "day 14" $ do
      it "works out the example for part a" $
        Day14.day14 ["flqrgnkx"] `shouldBe` 8108

      it "works out the cliques for the example" $ do
        let maze = Day14.mazeFromKey "flqrgnkx"
        Set.size maze `shouldBe` 8108
        let cs = Day14.cliques Set.empty maze
            first = Set.fromList [(0, 0), (1, 0), (1, 1)]
        Set.size cs `shouldBe` 1242
        Set.member first cs `shouldBe` True

    describe "day 15" $ do
      it "generates values" $ do
        take 6 (Day15.generator Day15.facA 65) `shouldBe` [65, 1092455, 1181022009, 245556042, 1744312007, 1352636452]
        take 6 (Day15.generator Day15.facB 8921) `shouldBe` [8921, 430625591, 1233683848, 1431495498, 137874439, 285222916]

      it "finds matches" $ do
        Day15.findPairs 40000000 (65, 8921) `shouldBe` 588
      it "finds matches using the slower generators" $ do
        Day15.findPairs' 5000000 (65, 8921) `shouldBe` 309

    describe "day 16" $ do
      forM_ [("s1", [Day16.Spin 1])
            ] $ \(i, o) -> do
        it ("parses " ++ i) $ do
          Day16.parse [i] `shouldBe` o

      let init = Day16.initState "abcde"
          prog = Day16.parse ["s1,x3/4,pe/b"]

      it "spins" $ do
        Day16.finish (Day16.move (Day16.initState "abcde") (Day16.Spin 3)) `shouldBe` "cdeab"

      it "runs the example dance" $ do
        let intermediates = scanl Day16.move init prog
            values = map Day16.finish intermediates
        values `shouldBe` ["abcde", "eabcd", "eabdc", "baedc"]

      it "runs a swapping dance" $ do
        let prog = Day16.parse ["pa/b,pa/b,pa/b,pb/c"]
            intermediates = scanl Day16.move init prog
            values = map Day16.finish intermediates
        values `shouldBe` ["abcde", "bacde", "abcde", "bacde", "cabde"]

      forM_ [ ("abcde", "a b c d e")
            , ("bacde", "ab c d e")
            , ("baecd", "ab ced")
            , ("acdeb", "a bcde")
            ] $ \(i,o) -> do
        it ("computes the circuits for swapping " ++ i) $ do
          Day16.circuits (Day16.initState i) `shouldBe` (words o, sort i & splitOn "" & filter (/=[]))

      forM_ [ ("pa/b,pb/c,pc/a", "acbde", "a bc d e")
            ] $ \(i,f,o) -> do
        it ("computes the circuits for partnering " ++ i) $ do
          let init = Day16.initState "abcde"
              prog = Day16.parse [i]
              final = Day16.run init prog
          Day16.finish final `shouldBe` f
          Day16.circuits final `shouldBe` (words "a b c d e", words o)

    describe "day 17" $ do
      it "runs the example" $ do
        let i = Day17.seq0
        (scanl (Day17.move 3) i [1..9] & map toList)
          `shouldBe` [ [0]
                     , [1, 0]
                     , [2, 1, 0]
                     , [3, 1, 0, 2]
                     , [4, 3, 1, 0, 2]
                     , [5, 2, 4, 3, 1, 0]
                     , [6, 1, 0, 5, 2, 4, 3]
                     , [7, 2, 4, 3, 6, 1, 0, 5]
                     , [8, 6, 1, 0, 5, 7, 2, 4, 3]
                     , [9, 5, 7, 2, 4, 3, 8, 6, 1, 0]
                     ]

    describe "day 18" $ do
      let example = "set a 1\n\
                    \add a 2\n\
                    \mul a a\n\
                    \mod a 5\n\
                    \snd a\n\
                    \set a 0\n\
                    \rcv a\n\
                    \jgz a -1\n\
                    \set a 1\n\
                    \jgz a -2" & lines
          prog = Day18.parse example
      it "runs the example" $ do
        Day18.run prog 0 Map.empty `shouldBe` Just 4
      
      
      let example = "snd 1\n\
                    \snd 2\n\
                    \snd p\n\
                    \rcv a\n\
                    \rcv b\n\
                    \rcv c\n\
                    \rcv d" & lines
          prog = Day18.parse example
      it "runs in parallel" $ do
        let p0 = (Map.empty, 0, [], [])
            p1 = (Map.singleton 'p' 1, 0, [], [])
            (p0', p1') = Day18.parallel1 prog p0 p1
            (p1'', p0'') = Day18.parallel2 prog p1 p0
        
        p0' `shouldBe` p0''
        p1' `shouldBe` p1''
        let (r0, _, [], s0) = p0'
            (r1, _, [], s1) = p1'
        
        r0 `shouldBe` Map.fromList [('a', 1), ('b', 2), ('c', 1)]
        r1 `shouldBe` Map.fromList [('a', 1), ('b', 2), ('c', 0), ('p', 1)]
        
        s0 `shouldBe` [1, 2, 0]
        s1 `shouldBe` [1, 2, 1]

    describe "day 19" $ do
      let example = "     |          \n\
                    \     |  +--+    \n\
                    \     A  |  C    \n\
                    \ F---|----E|--+ \n\
                    \     |  |  |  D \n\
                    \     +B-+  +--+ " & lines
          maze = Day19.parse example
      it "traces the example" $ do
        let (p0, d0) = Day19.start maze
        Day19.chase maze p0 d0 "" 0 `shouldBe` ("ABCDEF", 38)

    describe "day 20" $ do
      let example = "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>\n\    
                    \p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>\n\
                    \p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>\n\
                    \p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>" & lines
      it "runs the linear example" $ do
        Day20.day20b example `shouldBe` 1

    describe "day 21" $ do
      let g0 = loadMap ["abcd", "efgh", "ijkl", "mnop"]
          d1 = diceMap (2, 2) g0
      it "dices maps" $ do
        Map.size d1 `shouldBe` 4
        d1 Map.! (0,0) `shouldBe` loadMap ["ab", "ef"]
        d1 Map.! (1,0) `shouldBe` loadMap ["cd", "gh"]
        d1 Map.! (0,1) `shouldBe` loadMap ["ij", "mn"]
        d1 Map.! (1,1) `shouldBe` loadMap ["kl", "op"]
      it "undices maps" $ do
        undiceMap d1 `shouldBe` g0
      
      let example = "../.# => ##./#../...\n\
                    \.#./..#/### => #..#/..../..../#..#" & lines
          rules = Day21.parse example
          results = iterate (Day21.step rules) Day21.startGrid
      it "computes the rules" $ do
        let three = loadMap ["##.", "#..", "..."]
        rules Map.! (loadMap ["#.", ".."]) `shouldBe` three
        Map.size rules `shouldBe` 12  -- four for the 2x2, eight for the 3x3
      it "steps correctly" $ do
        results !! 1 `shouldBe` loadMap ["#..#", "....", "....", "#..#"]
        results !! 2 `shouldBe` loadMap ["##.##."
                                        ,"#..#.."
                                        ,"......"
                                        ,"##.##."
                                        ,"#..#.."
                                        ,"......"]
        Day21.gridCount (results !! 2) `shouldBe` 12

    describe "day 22" $ do
      let example = ["..#", "#..", "..."]
          (p, d, g) = Day22.parse example
          cells = g & Map.filter (=='#') & Map.map (const Day22.I)
          moves = iterate (Day22.step') (p, d, 0, cells)
      it "runs for 100 moves" $ do
        Day22.infections (moves !! 100) `shouldBe` 26

    describe "day 23" $ do
      let example = "0/2\n\
                    \2/2\n\
                    \2/3\n\
                    \3/4\n\
                    \3/5\n\
                    \0/1\n\
                    \10/1\n\
                    \9/10" & lines
          bridges = Day24.parse example
      it "works out the example" $ do
        let ans = Day24.srch 0 bridges
            strs = map Day24.strength ans
        maximum strs `shouldBe` 31

    describe "day 25" $ do
      it "parses" $ do
        let ex1 = "  If the current value is 0:\n\
                  \    - Write the value 1.\n\
                  \    - Move one slot to the right.\n\
                  \    - Continue with state B.\n"

        quickParse (Day25.tokenRule <* P.eof) ex1
          `shouldBe` Just (0, (1, Day25.R, 'B'))
        
        let ex2 = "In state A:\n\
                  \  If the current value is 0:\n\
                  \    - Write the value 1.\n\
                  \    - Move one slot to the right.\n\
                  \    - Continue with state B.\n\
                  \  If the current value is 1:\n\
                  \    - Write the value 0.\n\
                  \    - Move one slot to the left.\n\
                  \    - Continue with state E.\n"
        quickParse (Day25.stateRule <* P.eof) ex2
          `shouldBe` Just ('A', Map.fromList [
                           (0, (1, Day25.R, 'B')),
                           (1, (0, Day25.L, 'E'))])
                      