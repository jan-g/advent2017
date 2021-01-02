module Day8 where

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
--- Day 8: I Heard You Like Registers ---

You receive a signal directly from the CPU. Because of your recent assistance with jump instructions, it would like you to compute the result of a series of unusual register instructions.

Each instruction consists of several parts: the register to modify, whether to increase or decrease that register's value, the amount by which to increase or decrease it, and a condition. If the condition fails, skip the instruction without modifying the register. The registers all start at 0. The instructions look like this:

b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10

These instructions would be processed as follows:

    Because a starts at 0, it is not greater than 1, and so b is not modified.
    a is increased by 1 (to 1) because b is less than 5 (it is 0).
    c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
    c is increased by -20 (to -10) because c is equal to 10.

After this process, the largest value in any register is 1.

You might also encounter <= (less than or equal to) or != (not equal to). However, the CPU doesn't have the bandwidth to tell you what all the registers are named, and leaves that to you to determine.

What is the largest value in any register after completing the instructions in your puzzle input?

To begin, get your puzzle input.
-}

type Reg = String
data Instr = Inc Reg Integer Condition  deriving (Show, Eq)
data Condition = Eq Reg Integer | Ne Reg Integer | Ge Reg Integer | Le Reg Integer | Gt Reg Integer | Lt Reg Integer deriving (Show, Eq)

parse ls = ls
         & map (quickParse instr)
         & catMaybes

reg = munch1 isAlpha
minus x = -x
instr = (pure Inc <*> reg <* string " inc " <*> intParser <* string " if " <*> condition <* eof) <++
        (pure Inc <*> reg <* string " dec " <*> (pure minus <*> intParser) <* string " if " <*> condition <* eof)

condition = (pure Gt <*> reg <* string " > " <*> intParser) <++
            (pure Ge <*> reg <* string " >= " <*> intParser) <++
            (pure Lt <*> reg <* string " < " <*> intParser) <++
            (pure Le <*> reg <* string " <= " <*> intParser) <++
            (pure Eq <*> reg <* string " == " <*> intParser) <++
            (pure Ne <*> reg <* string " != " <*> intParser)


fetch regs r = fromMaybe 0 (Map.lookup r regs)

step regs (Inc r v cond) =
  if eval regs cond then Map.insert r (fetch regs r + v) regs else regs

eval regs (Eq r v) = fetch regs r == v
eval regs (Ne r v) = fetch regs r /= v
eval regs (Lt r v) = fetch regs r < v
eval regs (Le r v) = fetch regs r <= v
eval regs (Gt r v) = fetch regs r > v
eval regs (Ge r v) = fetch regs r >= v

run prog regs = foldl step regs prog

day8 ls =
  let prog = parse ls
      regs = run prog Map.empty
  in  maximum regs

{-
--- Part Two ---

To be safe, the CPU also needs to know the highest value held in any register during this process so that it can decide how much memory to allocate to these operations. For example, in the above instructions, the highest value ever held was 10 (in register c after the third instruction was evaluated).
-}

run' prog regs = foldl (\(m, r) i -> let r' = step r i
                                     in (max m (maximum r'), r'))
                       (0, regs) prog


day8b ls =
  let prog = parse ls
      (m, regs) = run' prog Map.empty
  in  m
