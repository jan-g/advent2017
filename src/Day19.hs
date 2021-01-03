module Day19 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))

import Lib


{-
--- Day 19: A Series of Tubes ---

Somehow, a network packet got lost and ended up here. It's trying to follow a routing diagram (your puzzle input), but it's confused about where to go.

Its starting point is just off the top of the diagram. Lines (drawn with |, -, and +) show the path it needs to take, starting by going down onto the only line connected to the top of the diagram. It needs to follow this path until it reaches the end (located somewhere within the diagram) and stop there.

Sometimes, the lines cross over each other; in these cases, it needs to continue going the same direction, and only turn left or right when there's no other option. In addition, someone has left letters on the line; these also don't change its direction, but it can use them to keep track of where it's been. For example:

     |
     |  +--+
     A  |  C
 F---|----E|--+
     |  |  |  D
     +B-+  +--+

Given this diagram, the packet needs to take the following path:

    Starting at the only line touching the top of the diagram, it must go down, pass through A, and continue onward to the first +.
    Travel right, up, and right, passing through B in the process.
    Continue down (collecting C), right, and up (collecting D).
    Finally, go all the way left through E and stopping at F.

Following the path to the end, the letters it sees on its path are ABCDEF.

The little packet looks up at you, hoping you can help it find the way. What letters will it see (in the order it would see them) if it follows the path? (The routing diagram is very wide; make sure you view it without line wrapping.)

To begin, get your puzzle input.
-}

parse ls = loadMap ls

day19 ls =
  let maze = parse ls
      (p0, d0) = start maze
  in chase maze p0 d0 []

start maze = let [((x0, y0), _)] = maze
                                 & Map.filterWithKey (\(x, y) c -> y == 0 && c == '|')
                                 & Map.toList
             in ((x0, y0), (0, 1))

chase maze (x,y) (dx, dy) seen =
  case Map.lookup (x, y) maze of
    Nothing -> seen
    Just '|' -> chase maze (x+dx, y+dy) (dx, dy) seen
    Just '-' -> chase maze (x+dx, y+dy) (dx, dy) seen
    Just '+' -> let lt = fromMaybe ' ' $ Map.lookup (x-dy, y+dx) maze
                    rt = fromMaybe ' ' $ Map.lookup (x+dy, y-dx) maze
                in if lt == ' ' then chase maze (x+dy, y-dx) (dy, -dx) seen
                                else chase maze (x-dy, y+dx) (-dy, dx) seen
    Just a   -> if isAlpha a
                then chase maze (x+dx, y+dy) (dx, dy) (seen ++ [a])
                else seen
                             
{-
-}

day19b ls = "hello world"
