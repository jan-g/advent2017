module Day18 where

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
--- Day 18: Duet ---

You discover a tablet containing some strange assembly code labeled simply "Duet". Rather than bother the sound card with it, you decide to run the code yourself. Unfortunately, you don't see any documentation, so you're left to figure out what the instructions mean on your own.

It seems like the assembly is meant to operate on a set of registers that are each named with a single letter and that can each hold a single integer. You suppose each register should start with a value of 0.

There aren't that many instructions, so it shouldn't be hard to figure out what they do. Here's what you determine:

    snd X plays a sound with a frequency equal to the value of X.
    set X Y sets register X to the value of Y.
    add X Y increases register X by the value of Y.
    mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
    mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
    rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
    jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)

Many of the instructions can take either a register (a single letter) or a number. The value of a register is the integer it contains; the value of a number is that number.

After each jump instruction, the program continues with the instruction to which the jump jumped. After any other instruction, the program continues with the next instruction. Continuing (or jumping) off either end of the program terminates it.

For example:

set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2

    The first four instructions set a to 1, add 2 to it, square it, and then set it to itself modulo 5, resulting in a value of 4.
    Then, a sound with frequency 4 (the value of a) is played.
    After that, a is set to 0, causing the subsequent rcv and jgz instructions to both be skipped (rcv because a is 0, and jgz because a is not greater than 0).
    Finally, a is set to 1, causing the next jgz instruction to activate, jumping back two instructions to another jump, which jumps again to the rcv, which ultimately triggers the recover operation.

At the time the recover operation is executed, the frequency of the last sound played is 4.

What is the value of the recovered frequency (the value of the most recently played sound) the first time a rcv instruction is executed with a non-zero value?

To begin, get your puzzle input.
-}

type Reg = Char
type Value = Integer
data Src = Reg Reg | Val Value deriving (Show, Eq)
data Instr = Snd Src | Set Reg Src | Add Reg Src | Mul Reg Src | Mod Reg Src | Rcv Reg | Jgz Reg Src
  deriving (Show, Eq)

parse :: [String] -> Map.Map Integer Instr
parse ls = ls
         & map (quickParse (line <* eof))
         & catMaybes
         & zip [0..]
         & Map.fromList

reg = satisfy isAlpha
src = (pure Reg <*> reg) <++ (pure Val <*> intParser)
line = (string "snd " *> pure Snd <*> src) <++
       (string "set " *> pure Set <*> reg <* char ' ' <*> src) <++
       (string "add " *> pure Add <*> reg <* char ' ' <*> src) <++
       (string "mul " *> pure Mul <*> reg <* char ' ' <*> src) <++
       (string "mod " *> pure Mod <*> reg <* char ' ' <*> src) <++
       (string "rcv " *> pure Rcv <*> reg) <++
       (string "jgz " *> pure Jgz <*> reg <* char ' ' <*> src)

sound = '~'

run :: Map.Map Integer Instr -> Integer -> Map.Map Reg Value -> Maybe Value
run prog pc regs =
  case Map.lookup pc prog of
    Nothing -> Nothing
    Just (Snd src) -> run prog (pc + 1) (store sound (eval src))
    Just (Set r src) -> run prog (pc + 1) (store r (eval src))
    Just (Add r src) -> run prog (pc + 1) (store r (fetch r + eval src))
    Just (Mul r src) -> run prog (pc + 1) (store r (fetch r * eval src))
    Just (Mod r src) -> run prog (pc + 1) (store r (fetch r `mod` eval src))
    Just (Rcv r)     -> if fetch r == 0
                        then run prog (pc + 1) regs
                        else Just $ fetch sound
    Just (Jgz r src) -> if fetch r > 0
                        then run prog (pc + eval src) regs
                        else run prog (pc + 1) regs
  where
    fetch reg = fromMaybe 0 $ Map.lookup reg regs
    eval (Reg r) = fetch r
    eval (Val v) = v
    store r v = Map.insert r v regs

--
--    snd X plays a sound with a frequency equal to the value of X.
--    set X Y sets register X to the value of Y.
--    add X Y increases register X by the value of Y.
--    mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
--    mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
--    rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
--    jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)

day18 ls =
  let prog = parse ls
  in  run prog 0 Map.empty

{-
-}

day18b ls = "hello world"
