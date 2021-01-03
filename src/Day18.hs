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
import Debug.Trace (trace)

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
data Instr = Snd Src | Set Reg Src | Add Reg Src | Mul Reg Src | Mod Reg Src | Rcv Reg | Jgz Src Src
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
       (string "jgz " *> pure Jgz <*> src <* char ' ' <*> src)

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
    Just (Jgz tst src) -> if eval tst > 0
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
--- Part Two ---

As you congratulate yourself for a job well done, you notice that the documentation has been on the back of the tablet this entire time. While you actually got most of the instructions correct, there are a few key differences. This assembly code isn't about sound at all - it's meant to be run twice at the same time.

Each running copy of the program has its own set of registers and follows the code independently - in fact, the programs don't even necessarily run at the same speed. To coordinate, they use the send (snd) and receive (rcv) instructions:

    snd X sends the value of X to the other program. These values wait in a queue until that program is ready to receive them. Each program has its own message queue, so a program can never receive a message it sent.
    rcv X receives the next value and stores it in register X. If no values are in the queue, the program waits for a value to be sent to it. Programs do not continue to the next instruction until they have received a value. Values are received in the order they are sent.

Each program also has its own program ID (one 0 and the other 1); the register p should begin with this value.

For example:

snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d

Both programs begin by sending three values to the other. Program 0 sends 1, 2, 0; program 1 sends 1, 2, 1. Then, each program receives a value (both 1) and stores it in a, receives another value (both 2) and stores it in b, and then each receives the program ID of the other program (program 0 receives 1; program 1 receives 0) and stores it in c. Each program now sees a different value in its own copy of register c.

Finally, both programs try to rcv a fourth time, but no data is waiting for either of them, and they reach a deadlock. When this happens, both programs terminate.

It should be noted that it would be equally valid for the programs to run at different speeds; for example, program 0 might have sent all three values and then stopped at the first rcv before program 1 executed even its first instruction.

Once both of your programs have terminated (regardless of what caused them to do so), how many times did program 1 send a value?
-}


type Regs = Map.Map Reg Value
type PC = Integer
type Prog = Map.Map Integer Instr
data Status = Send Regs PC Value | Receive Regs PC Reg | Halt Regs PC deriving (Show, Eq)

step :: Prog -> PC -> Regs -> Status
step prog pc regs =
  case Map.lookup pc prog of
    Nothing -> Halt regs pc
    Just (Snd src) -> Send regs (pc + 1) (eval src)
    Just (Set r src) -> step prog (pc + 1) (store r (eval src))
    Just (Add r src) -> step prog (pc + 1) (store r (fetch r + eval src))
    Just (Mul r src) -> step prog (pc + 1) (store r (fetch r * eval src))
    Just (Mod r src) -> step prog (pc + 1) (store r (fetch r `mod` eval src))
    Just (Rcv r)     -> Receive regs pc r
    Just (Jgz tst src) -> if eval tst > 0
                        then step prog (pc + eval src) regs
                        else step prog (pc + 1) regs
  where
    fetch reg = fromMaybe 0 $ Map.lookup reg regs
    eval (Reg r) = fetch r
    eval (Val v) = v
    store r v = Map.insert r v regs

parallel1 prog p1@(rs1, pc1, q1, s1) p2@(rs2, pc2, q2, s2) =
--  trace ("stepping in prog1 at " ++ show pc1) $
  case step prog pc1 rs1 of
    Send rs' pc' v -> parallel1 prog (rs', pc', q1, s1 ++ [v]) (rs2, pc2, q2 ++ [v], s2)
    Receive rs' pc' r -> if null q1
                           then
--                                trace ("prog1 pauses at " ++ show pc' ++ " for input") $
                                parallel2 prog (rs', pc', q1, s1) (rs2, pc2, q2, s2)
                           else parallel1 prog (rs' & Map.insert r (head q1), pc' + 1, tail q1, s1)
                                               (rs2, pc2, q2, s2)
    Halt regs' pc' -> trace "prog1 halts" $
                      ((regs', pc', q1, s1), p2)

parallel2 prog p1@(rs1, pc1, q1, s1) p2@(rs2, pc2, q2, s2) =
  -- we only get here if waiting for input in prog1
--  trace ("stepping in prog2 at " ++ show pc2) $
  case step prog pc2 rs2 of
    -- as soon as something is sent, kick off prog1 again
    Send rs' pc' v -> parallel1 prog (rs1, pc1, q1 ++ [v], s1) (rs', pc', q2, s2 ++ [v])
    Receive rs' pc' r -> if null q2
                           then
--                                trace "prog1 and prog2 both waiting on input" $
                                (p1, (rs', pc', q2, s2))  -- th are waiting on input
                           else parallel2 prog (rs1, pc1, q1, s1)
                                               (rs' & Map.insert r (head q2), pc' + 1, tail q2, s2)
    Halt regs' pc' -> trace "prog2 halts" $
                      (p1, (regs', pc', q2, s2))

day18b ls =
  let prog = parse ls
      p0 = (Map.empty, 0, [], [])
      p1 = (Map.singleton 'p' 1, 0, [], [])
      (s1, s0) = parallel1 prog p1 p0
      (s0', s1') = parallel1 prog p0 p1
  in trace (map (show . snd) (Map.toList prog) & unlines) $
     (s0 == s0', s1 == s1', length (sent s0), length (sent s1))
  where sent (_, _, _, s) = s
