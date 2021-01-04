module Day23 where

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
--- Day 23: Coprocessor Conflagration ---

You decide to head directly to the CPU and fix the printer from there. As you get close, you find an experimental coprocessor doing so much work that the local programs are afraid it will halt and catch fire. This would cause serious issues for the rest of the computer, so you head in and see what you can do.

The code it's running seems to be a variant of the kind you saw recently on that tablet. The general functionality seems very similar, but some of the instructions are different:

    set X Y sets register X to the value of Y.
    sub X Y decreases register X by the value of Y.
    mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
    jnz X Y jumps with an offset of the value of Y, but only if the value of X is not zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)

    Only the instructions listed above are used. The eight registers here, named a through h, all start at 0.

The coprocessor is currently set to some kind of debug mode, which allows for testing, but prevents it from doing any meaningful work.

If you run the program (your puzzle input), how many times is the mul instruction invoked?

To begin, get your puzzle input.
-}

type Reg = Char
type Value = Integer
data Src = Reg Reg | Val Value deriving (Show, Eq)
data Instr = Set Reg Src | Sub Reg Src | Mul Reg Src | Jnz Src Src deriving (Show, Eq)
type Prog = Map.Map Integer Instr

parse ls = ls
         & map (quickParse (line <* eof))
         & catMaybes
         & zip [0..]
         & Map.fromList

reg = satisfy isAlpha
val = intParser
src = (pure Reg <*> reg) <++ (pure Val <*> val)
line = (string "set " *> pure Set <*> reg <* char ' ' <*> src) <++
       (string "sub " *> pure Sub <*> reg <* char ' ' <*> src) <++
       (string "mul " *> pure Mul <*> reg <* char ' ' <*> src) <++
       (string "jnz " *> pure Jnz <*> src <* char ' ' <*> src)

run prog pc regs mulCount =
  case Map.lookup pc prog of
    Nothing -> mulCount
    Just (Set r s) -> run prog (pc + 1) (store r (eval s)) mulCount
    Just (Sub r s) -> run prog (pc + 1) (store r (fetch r - eval s)) mulCount
    Just (Mul r s) -> run prog (pc + 1) (store r (fetch r * eval s)) (mulCount + 1)
    Just (Jnz s t) -> if eval s /= 0
                      then run prog (pc + eval t) regs mulCount
                      else run prog (pc + 1) regs mulCount
  where
    eval (Val v) = v
    eval (Reg r) = fetch r
    fetch r = fromMaybe 0 $ Map.lookup r regs
    store r v = regs & Map.insert r v
  
day23 ls =
  let prog = parse ls
  in  run prog 0 Map.empty 0

{-
-}

day23b ls = "hello world"
