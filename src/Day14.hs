{-# LANGUAGE TupleSections #-}

module Day14 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt, showHex, showIntAtBase)
import Data.Bits ((.&.), (.|.))
import Data.Text.Read (hexadecimal)
import Data.Text (pack)
import Text.Printf (printf)

import Debug.Trace (trace)

import Lib

import qualified Day10

{-
--- Day 14: Disk Defragmentation ---

Suddenly, a scheduled job activates the system's disk defragmenter. Were the situation different, you might sit and watch it for a while, but today, you just don't have that kind of time. It's soaking up valuable system resources that are needed elsewhere, and so the only option is to help it finish its task as soon as possible.

The disk in question consists of a 128x128 grid; each square of the grid is either free or used. On this disk, the state of the grid is tracked by the bits in a sequence of knot hashes.

A total of 128 knot hashes are calculated, each corresponding to a single row in the grid; each hash contains 128 bits which correspond to individual grid squares. Each bit of a hash indicates whether that square is free (0) or used (1).

The hash inputs are a key string (your puzzle input), a dash, and a number from 0 to 127 corresponding to the row. For example, if your key string were flqrgnkx, then the first row would be given by the bits of the knot hash of flqrgnkx-0, the second row from the bits of the knot hash of flqrgnkx-1, and so on until the last row, flqrgnkx-127.

The output of a knot hash is traditionally represented by 32 hexadecimal digits; each of these digits correspond to 4 bits, for a total of 4 * 32 = 128 bits. To convert to bits, turn each hexadecimal digit to its equivalent binary value, high-bit first: 0 becomes 0000, 1 becomes 0001, e becomes 1110, f becomes 1111, and so on; a hash that begins with a0c2017... in hexadecimal would begin with 10100000110000100000000101110000... in binary.

Continuing this process, the first 8 rows and columns for key flqrgnkx appear as follows, using # to denote used squares, and . to denote free ones:

##.#.#..-->
.#.#.#.#
....#.#.
#.#.##.#
.##.#...
##..#..#
.#...#..
##.#.##.-->
|      |
V      V

In this example, 8108 squares are used across the entire 128x128 grid.

Given your actual key string, how many squares are used?

Your puzzle input is xlqgujun.
-}


parse ls = head ls

day14 ls =
  let key = parse ls
  in  Set.size (mazeFromKey key)


{-
--- Part Two ---

Now, all the defragmenter needs to know is the number of regions. A region is a group of used squares that are all adjacent, not including diagonals. Every used square is in exactly one region: lone used squares form their own isolated regions, while several adjacent squares all count as a single region.

In the example above, the following nine regions are visible, each marked with a distinct digit:

11.2.3..-->
.1.2.3.4
....5.6.
7.8.55.9
.88.5...
88..5..8
.8...8..
88.8.88.-->
|      |
V      V

Of particular interest is the region marked 8; while it does not appear contiguous in this small view, all of the squares marked 8 are connected when considering the whole 128x128 grid. In total, in this example, 1242 regions are present.

How many regions are present given your key string?
-}

day14b ls =
  let key = parse ls
      maze = mazeFromKey key
      c = cliques Set.empty maze
  in c & Set.size

mazeFromKey key = [0..127]
                & map (\k -> let kk = key ++ "-" ++ show k
                                 hash = Day10.day10b [kk]
                             in hexToBinary 128 hash)
                & loadMap
                & Map.filter (=='1')
                & Map.keysSet

cliques found maze =
  case Set.lookupMin maze of
    Nothing -> found
    Just (x, y) ->
      let Left clique = flood (next maze) (const False) id (0, (x, y))
      in trace ("found clique: " ++ show clique) $ 
         cliques (Set.insert clique found) (Set.difference maze clique)

next maze (c, (x, y)) = Set.fromList (neighbours orthogonalMoves (x, y))
                      & Set.intersection maze
                      & Set.map (c + 1,)


hexToBinary len hex =
 take (len - length bits) (repeat '0') ++ bits
  where
    val :: Integer
    Right (val, _) = hexadecimal (pack hex)
    bits = showIntAtBase 2 intToDigit val ""
