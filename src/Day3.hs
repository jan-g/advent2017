module Day3 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))

import Debug.Trace (trace)

import Lib


{-
--- Day 3: Spiral Memory ---

You come across an experimental new kind of memory stored on an infinite two-dimensional grid.

Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.

For example:

    Data from square 1 is carried 0 steps, since it's at the access port.
    Data from square 12 is carried 3 steps, such as: down, left, left.
    Data from square 23 is carried only 2 steps: up twice.
    Data from square 1024 must be carried 31 steps.

How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?

Your puzzle input is 312051.
-}

parse :: [String] -> Integer
parse ls = ls
         & head
         & read

ringOf pos = dropWhile ((< pos) . fst) [(j ^ 2, i) | i <- [1..], let j = i * 2 - 1] & head & snd
circuit pos = let innerRing = ringOf pos - 1
                  size = innerRing * 2 - 1
                  squares = size ^ 2
              in max 0 $ pos - squares - 1
sideLength pos = 2 * ringOf pos - 1

position 1 = (0, 0)
position pos =
  let ring = ringOf pos
      circum = circuit pos
      len = sideLength pos - 1
      circumference = len * 4
      place = (circum + 1) `mod` circumference   -- 0 is bottom right
      offset = place `mod` len
  in  --trace ("position " ++ (show pos) ++ " ring=" ++ (show ring) ++ " circum=" ++ (show circum) ++ " len=" ++ (show len) ++
      --       " place=" ++ (show place) ++ " offset=" ++ (show offset) ) $
      if place < len then          -- trace ("right side") $
                                        (ring - 1, 1 - ring + offset)
      else if place < len * 2 then -- trace ("top side") $
                                        (ring - 1 - offset, ring - 1)
      else if place < len * 3 then -- trace ("left side") $
                                        (1 - ring, ring - 1 - offset)
      else                         -- trace ("bottom side") $
                                        (1 - ring + offset, 1 - ring)

manh (x,y) = abs x + abs y

day3 ls = let pos = parse ls
          in manh (position pos)


{-
--- Part Two ---

As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.

So, the first few squares' values are chosen as follows:

    Square 1 starts with the value 1.
    Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
    Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
    Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
    Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.

Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...

What is the first value written that is larger than your puzzle input?
-}

day3b ls =
  let v = parse ls
      grids = scanl (\(last, grid) p -> let (x, y) = position p
                                            values = [Map.lookup (x+dx, y+dy) grid | (dx,dy) <- [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]]
                                            s = catMaybes values & sum
                                        in  -- trace ("at position " ++ show p ++ " = " ++ show (x,y) ++ " storing " ++ show s ++ show values) $
                                            (s, Map.insert (x,y) s grid))
                 (1, Map.singleton (0,0) 1)
                 [2..]
  in grids & dropWhile (\(s, g) -> s <= v) & head & fst