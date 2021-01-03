module Day16 where

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
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

import Debug.Trace (trace)

import Lib


{-
--- Day 16: Permutation Promenade ---

You come upon a very unusual sight; a group of programs here appear to be dancing.

There are sixteen programs in total, named a through p. They start by standing in a line: a stands in position 0, b stands in position 1, and so on until p, which stands in position 15.

The programs' dance consists of a sequence of dance moves:

    Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For example, s3 on abcde produces cdeab).
    Exchange, written xA/B, makes the programs at positions A and B swap places.
    Partner, written pA/B, makes the programs named A and B swap places.

For example, with only five programs standing in a line (abcde), they could do the following dance:

    s1, a spin of size 1: eabcd.
    x3/4, swapping the last two programs: eabdc.
    pe/b, swapping programs e and b: baedc.

After finishing their dance, the programs end up in order baedc.

You watch the dance for a while and record their dance moves (your puzzle input). In what order are the programs standing after their dance?
-}

{-
The basic idea here is that spins and exchanges are independent of partner swaps. In other words,
we can leave all the partner swaps until the end and then process them, having done all the
spins. Ie, (spins + exchanges) commute with (partner swaps).

For the second part, therefore, we can potentially run a billion lots of the (spins + exchanges)
followed by a billion of the (partner swaps).
However, these are both permutations. We look for cycles in those permutations, and use the 
LCM of their lengths - that gets the period over which the dance will return to its original
ordering (which is much less than a billion).
It's then trivial to run the dance for the remainder of the iterations (rather than being clever
and reassembling the sequences, we just iterate for what is less than 100 times).
-}

parse ls = ls
         & map (quickParse line)
         & catMaybes
         & concat

type Dancer = Char
data Instr = Spin Int | Exchange Int Int | Partner Dancer Dancer deriving (Show, Eq)

i = pure fromIntegral <*> natParser
p = satisfy isAlpha

line = P.sepBy (
        (char 's' *> pure Spin <*> i) <++
        (char 'x' *> pure Exchange <*> i <* char '/' <*> i) <++
        (char 'p' *> pure Partner <*> p  <* char '/' <*> p)
       ) (char ',') <* eof

initState s = (Seq.fromList s,
               Map.fromList $ zip s s)

move (seq, part) (Spin n) = let (a, b) = Seq.splitAt ((-n) `mod` Seq.length seq) seq
                            in (b Seq.>< a, part)
move (seq, part) (Exchange x y) = let (p, q) = (min x y, max x y)
                                      (j, a Seq.:<| kl) = Seq.splitAt p seq
                                      (k, b Seq.:<| l) = Seq.splitAt (q - p - 1) kl
                                  in (j Seq.>< b Seq.<| k Seq.>< a Seq.<| l, part)
move (seq, part) (Partner p q) = (seq, part & Map.insert p (part Map.! q)
                                            & Map.insert q (part Map.! p))            

pad n s = (take (n - length s) (repeat ' ')) ++ s
run state moves = foldl (\s m -> let s' = move s m
                                 in -- trace ((pad 20 $ show m) ++ "\t" ++ finish s') $
                                    s') state moves

finish (seq, part) = let back = part & Map.toList & map swap & Map.fromList in
                     toList seq & map (back Map.!)

day16 ls =
  let init = initState ['a'..'p']
      prog = parse ls
  in trace ("starting state:" ++ finish init)
     finish $ run init prog

{-
--- Part Two ---

Now that you're starting to get a feel for the dance moves, you turn your attention to the dance as a whole.

Keeping the positions they ended up in from their previous dance, the programs perform it again and again: including the first dance, a total of one billion (1000000000) times.

In the example above, their second dance would begin with the order baedc, and use the same dance moves:

    s1, a spin of size 1: cbaed.
    x3/4, swapping the last two programs: cbade.
    pe/b, swapping programs e and b: ceadb.

In what order are the programs standing after their billion dances?
-}

circuits (seq, part) =
  -- work out the cycles this dance creates in positional changes
  let f = finish (seq, part)
      chars = sort f
      moveMap = zip chars (toList seq) & Map.fromList  -- starting character -> position
      moveCycles = findCycles [] moveMap
      part' = part & Map.toList & map swap & Map.fromList
      partCycles = findCycles [] part'
  in (moveCycles, partCycles)
  where
    findCycles :: [String] -> Map.Map Char Char -> [String]
    findCycles found remaining =
      case Map.minViewWithKey remaining of
        Nothing -> found
        Just ((from, to), _) -> let seq = iterate (remaining Map.!) from
                                        & tail
                                        & takeWhile (/= from)
                                        & (from:)
                                in findCycles
                                     (found ++ [seq])
                                     (Map.filterWithKey (\k c -> not $ k `elem` seq) remaining)



day16b ls =
  let init = initState ['a'..'p']
      prog = parse ls
      once = run init prog
      (moves, parts) = circuits once
      cycleLength = foldl1 lcm (map (fromIntegral . length) $ moves ++ parts)   :: Integer
      remainder = 1000000000 `mod` cycleLength
      progn = take (fromIntegral remainder) (repeat prog) & concat
  in (moves, parts, finish (run init progn))
