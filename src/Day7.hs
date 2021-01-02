module Day7 where

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
import qualified Data.Counter as C
import Debug.Trace (trace)

import Lib


{-
--- Day 7: Recursive Circus ---

Wandering further through the circuits of the computer, you come upon a tower of programs that have gotten themselves into a bit of trouble. A recursive algorithm has gotten out of hand, and now they're balanced precariously in a large tower.

One program at the bottom supports the entire tower. It's holding a large disc, and on the disc are balanced several more sub-towers. At the bottom of these sub-towers, standing on the bottom disc, are other programs, each holding their own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many programs stand simply keeping the disc below them balanced but with no disc of their own.

You offer to help, but first you need to understand the structure of these towers. You ask each program to yell out their name, their weight, and (if they're holding a disc) the names of the programs immediately above them balancing on that disc. You write this information down (your puzzle input). Unfortunately, in their panic, they don't do this in an orderly fashion; by the time you're done, you're not sure which program gave which information.

For example, if your list is the following:

pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)

...then you would be able to recreate the structure of the towers that looks like this:

                gyxo
              /
         ugml - ebii
       /      \
      |         jptl
      |
      |         pbga
     /        /
tknk --- padx - havc
     \        \
      |         qoyq
      |
      |         ktlj
       \      /
         fwft - cntj
              \
                xhth

In this example, tknk is at the bottom of the tower (the bottom program), and is holding up ugml, padx, and fwft. Those programs are, in turn, holding up other programs; in this example, none of those programs are holding up any other programs, and are all the tops of their own towers. (The actual tower balancing in front of you is much larger.)

Before you're ready to help them, you need to make sure your information is correct. What is the name of the bottom program?

To begin, get your puzzle input.
-}

parse ls = ls
         & map (quickParse tower)
         & catMaybes
         & Map.fromList

tower = leaf <++ trunk

leaf = do
  node <- munch1 isAlpha
  string " ("
  weight <- natParser
  string ")"
  eof
  return (node, (weight, Set.empty))

trunk = do
  node <- munch1 isAlpha
  string " ("
  weight <- natParser
  string ") -> "
  above <- P.sepBy (munch1 isAlpha) (string ", ")
  eof
  return (node, (weight, Set.fromList above))



base :: Map.Map String (Integer, Set.Set String) -> String
base tower =
  if Map.size tower == 1 then Map.toList tower & head & fst
  else
    let singles = tower & Map.filter (\(_, s) -> Set.null s) & Map.keysSet
        -- drop those leaves
        t' = tower & Map.filterWithKey (\k _ -> not $ Set.member k singles)
           & Map.map (\(w, s) -> (w, s `Set.difference` singles))
    in base t'

day7 ls = parse ls & base

{-
--- Part Two ---

The programs explain the situation: they can't get down. Rather, they could get down, if they weren't expending all of their energy trying to keep the tower balanced. Apparently, one program has the wrong weight, and until it's fixed, they're stuck here.

For any program holding a disc, each program standing on that disc forms a sub-tower. Each of those sub-towers are supposed to be the same weight, or the disc itself isn't balanced. The weight of a tower is the sum of the weights of the programs in that tower.

In the example above, this means that for ugml's disc to be balanced, gyxo, ebii, and jptl must all have the same weight, and they do: 61.

However, for tknk to be balanced, each of the programs standing on its disc and all programs above it must each match. This means that the following sums must all be the same:

    ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
    padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
    fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243

As you can see, tknk's disc is unbalanced: ugml's stack is heavier than the other two. Even though the nodes above ugml are balanced, ugml itself is too heavy: it needs to be 8 units lighter for its stack to weigh 243 and keep the towers balanced. If this change were made, its weight would be 60.

Given that exactly one program is the wrong weight, what would its weight need to be to balance the entire tower?
-}

data Tree = Node String Integer (Set.Set Tree) deriving (Show, Eq, Ord)
name (Node n _ _) = n
children (Node _ _ ns) = ns
weight (Node _ w _) = w

buildTree :: Map.Map String (Integer, Set.Set String) -> String -> Tree
buildTree tower node =
  let (w, s) = tower Map.! node
  in  Node node w $ Set.map (buildTree tower) s

totalWeight (Node _ w ns) = w + sum (Set.toList ns & map totalWeight)

balanced (Node _ w ns) = Set.size (Set.map totalWeight ns) == 1

huntUnbalanced delta n =
  trace ("looking in node " ++ name n ++ " for delta " ++ show delta) $
  if balanced n then (delta, n)    -- if there's a problem, it's with this node
  else -- work out the child that's too light or too heavy
    let childWeights = Set.toList (children n) & map (\c -> (c, totalWeight c)) & Map.fromList
        weights = Map.toList childWeights & map snd & C.count
        [(wonky, 1)] = weights & Map.filter (==1) & Map.toList
        [(bustNode, _)] = Map.filter (== wonky) childWeights & Map.toList
        [(goodWeight, _)] = weights & Map.filter (/= 1) & Map.toList
    in huntUnbalanced (goodWeight - wonky) bustNode

day7b ls =
  let tower = parse ls
      b = base tower
      node = buildTree tower b
  in
    trace (show $ Set.map (\n -> (name n, totalWeight n)) (children node)) $
    let (delta, bust) = huntUnbalanced 0 node
    in  weight bust + delta
