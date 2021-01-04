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
import Data.Numbers.Primes (isPrime)

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
data Instr = Set Reg Src | Sub Reg Src | Mul Reg Src | Jnz Src Src
  deriving (Show, Eq)
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
--- Part Two ---

Now, it's time to fix the problem.

The debug mode switch is wired directly to register a. You flip the switch, which makes register a now start at 1 when the program is executed.

Immediately, the coprocessor begins to overheat. Whoever wrote this program obviously didn't choose a very efficient implementation. You'll need to optimize the program if it has any hope of completing before Santa needs that printer working.

The coprocessor's ultimate goal is to determine the final value left in register h once the program completes. Technically, if it had that... it wouldn't even need to run the program.

After setting register a to 1, if the program were to run to completion, what value would be left in register h?
-}


searchAndReplace1 seq1 seq2 as
  | null as     = []
  | take (length seq1) as == seq1 = seq2 ++ (drop (length seq1) as)
  | otherwise   = (head as):searchAndReplace1 seq1 seq2 (tail as)


day23b ls =
  let b0 = 99 * 100 + 100000
      c0 = b0 + 17000
      ns = [b0, b0+17 .. c0]
  in  length [i | i <- ns, not $ isPrime i]

{-
set b 99          b = 99
set c b           c = b
jnz a 2           if a:
jnz 1 5
mul b 100             b *= 100          b = 9900
sub b -100000         b += 100000       b = 109900
set c b               c = b             c = 109900
sub c -17000          c += 17000        c = 126900
                LOOP0:
set f 1           f = 1
set d 2           d = 2
set e 2           e = 2
                LOOP1:
set g d           g = d
mul g e           g *= e
sub g b           g -= b
jnz g 2           if g == 0:            if d * e = b, f = 0
set f 0               f = 0
sub e -1          e += 1
set g e           g = e
sub g b           g -= b
jnz g -8          if g: goto LOOP1      LOOP1 to here: above, g, f, e altered
                                        f = 0 => d is a factor of b
sub d -1          d += 1
set g d           g = d
sub g b           g -= b
jnz g -13         if g: goto LOOP1      LOOP1 to here: g, d altered
jnz f 2           if f == 0:            f = 0 => b is composite
sub h -1              h += 1            h counts these
set g b           g = b
sub g c           g -= c
jnz g 2           if g:                 while b < c:
jnz 1 3
sub b -17           b += 17               b += 17
jnz 1 -23           goto LOOP0

In other words: counts the number of composite numbers between b0 and c0, inclusive,
  looking at increments of 17 each time.
-}
