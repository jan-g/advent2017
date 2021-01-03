module Day11 where

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

import Lib


{-
--- Day 11: Hex Ed ---

Crossing the bridge, you've barely reached the other side of the stream when a program comes up to you, clearly in distress. "It's my child process," she says, "he's gotten lost in an infinite grid!"

Fortunately for her, you have plenty of experience with infinite grids.

Unfortunately for you, it's a hex grid.

The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be found to the north, northeast, southeast, south, southwest, and northwest:

  \ n  /
nw +--+ ne
  /    \
-+      +-
  \    /
sw +--+ se
  / s  \

You have the path the child process took. Starting where he started, you need to determine the fewest number of steps required to reach him. (A "step" means to move from the hex you are in to any adjacent hex.)

For example:

    ne,ne,ne is 3 steps away.
    ne,ne,sw,sw is 0 steps away (back where you started).
    ne,ne,s,s is 2 steps away (se,se).
    se,sw,se,sw,sw is 3 steps away (s,s,sw).

To begin, get your puzzle input.
-}

parse ls = ls
         & head
         & splitOn ","

{-
  \ n  /                      constant a  ->> increases
nw +--+ ne                    |
  /    \        increases     |
-+      +-        \          / \
  \    /           \        /   \
sw +--+ se         constant b    constant c
  / s  \                              /
                                     /
                                increases

  We count the distance *west* as positive a
  We count the distance se/nw as positive/negative b
  We count the distnace ne/sw as positive/negative c
-}

delta "n" = (0, 1, -1)
delta "s" = (0, -1, 1)
delta "ne" = (1, 0, -1)
delta "sw" = (-1, 0, 1)
delta "nw" = (-1, 1, 0)
delta "se" = (1, -1, 0)

move (x, y, z) d = let (dx, dy, dz) = delta d in (x+dx, y+dy, z+dz)

follow (x, y, z) moves = foldl move (x, y, z) moves 

manh (x, y, z) = let [a, b, c] = sort [abs x, abs y, abs z]
                 in a + b

day11 ls =
  let moves = parse ls
  in  manh (follow (0,0,0) moves)

{-
--- Part Two ---

How many steps away is the furthest he ever got from his starting position?
-}

day11b ls =
  let moves = parse ls
      path = scanl move (0,0,0) moves
      dists = map manh path
  in maximum dists