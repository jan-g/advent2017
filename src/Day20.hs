module Day20 where

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
import Debug.Trace (trace)

import Lib


{-
--- Day 20: Particle Swarm ---

Suddenly, the GPU contacts you, asking for help. Someone has asked it to simulate too many particles, and it won't be able to finish them all in time to render the next frame at this rate.

It transmits to you a buffer (your puzzle input) listing each particle in order (starting with particle 0, then particle 1, particle 2, and so on). For each particle, it provides the X, Y, and Z coordinates for the particle's position (p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.

Each tick, all particles are updated simultaneously. A particle's properties are updated in the following order:

    Increase the X velocity by the X acceleration.
    Increase the Y velocity by the Y acceleration.
    Increase the Z velocity by the Z acceleration.
    Increase the X position by the X velocity.
    Increase the Y position by the Y velocity.
    Increase the Z position by the Z velocity.

Because of seemingly tenuous rationale involving z-buffering, the GPU would like to know which particle will stay closest to position <0,0,0> in the long term. Measure this using the Manhattan distance, which in this situation is simply the sum of the absolute values of a particle's X, Y, and Z position.

For example, suppose you are only given two particles, both of which stay entirely on the X-axis (for simplicity). Drawing the current states of particles 0 and 1 (in that order) with an adjacent a number line and diagram of current X positions (marked in parentheses), the following would take place:

p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)

At this point, particle 1 will never be closer to <0,0,0> than particle 0, and so, in the long run, particle 0 will stay closest.

Which particle will stay closest to position <0,0,0> in the long term?

To begin, get your puzzle input.
-}

type Coord = (Integer, Integer, Integer)
data Particle = P { pos :: Coord, vel :: Coord, acc :: Coord } deriving (Show, Eq)

parse ls = ls
         & map (quickParse (line <* eof))
         & catMaybes

trio a b c = (a, b, c)
coord = between (char '<') (char '>') (pure trio <*> intParser <* char ',' <*> intParser <* char ',' <*> intParser)
part p v a = P { pos=p, vel=v, acc=a }
line = string "p=" *> pure part <*> coord <* string ", v=" <*> coord <* string ", a=" <*> coord

manh (a, b, c) = abs a + abs b + abs c

day20 ls = parse ls
         & zip [0..]
         & minimumBy (\(_, p) (_, q) -> manh (acc p) `compare` manh (acc q))


{-
--- Part Two ---

To simplify the problem further, the GPU would like to remove any particles that collide. Particles collide if their positions ever exactly match. Because particles are updated simultaneously, more than two particles can collide at the same time and place. Once particles collide, they are removed and cannot collide with anything else after that tick.

For example:

p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>

p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>
p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)
p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>

p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>
p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)
p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>

------destroyed by collision------
------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
------destroyed by collision------                      (3)
p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>

In this example, particles 0, 1, and 2 are simultaneously destroyed at the time and place marked X. On the next tick, particle 3 passes through unharmed.

How many particles are left after all collisions are resolved?
-}

{- At time t:
   a = a0
   v = v0 + ta
   p = p0 + tv0 + a + 2a + ... ta =
     = p0 + tv0 + t(1 + t)/2 . a

Two particles collide iff:
  p, v, a    and    q, w, b
  p + vt + at(1 + t)/2 = q + wt + bt(1 + t)/2
  or
  0 = (p - q) + (v - w)t + (a - b) t(1 + t)/2
  ... find integer solutions for all three dimensions. If there's a minimal t >= 0 then that's the point of collision
-}

x (a, b, c) = a
y (a, b, c) = b
z (a, b, c) = c

-- Find integer solutions for 0 = a + bt + ct^2
solve2 :: Integer -> Integer -> Integer -> Set.Set Integer
solve2 a b c =
  if a == 0 then (solve1 b c) `Set.union` (Set.singleton 0)
  else
  let s = b^2 - 4 * a * c
  in  -- trace ("taking square root of " ++ show s) $
      case srt s of
        Nothing -> Set.empty
        Just b2 -> -- trace ("b^2 - 4ac has sqrt " ++ show b2) $
                   let (s1, s2) = (-b + b2, -b - b2)
                       (q1, r1) = s1 `divMod` (2 * c)
                       (q2, r2) = s2 `divMod` (2 * c)
                   in  Set.union
                         (if r1 == 0 then Set.singleton q1 else Set.empty)
                         (if r2 == 0 then Set.singleton q2 else Set.empty)

-- find integer solutions for 0 = a + bt
solve1 a b =
  let (q, r) = ((-a) `divMod` b)
  in  if r == 0 then Set.singleton q else Set.empty

data Solutions = Some (Set.Set Integer) | All deriving (Show, Eq, Ord)

-- various solutions
solve a b c =
  -- trace ("solving " ++ show a ++ " + " ++ show b ++ "t + " ++ show c ++ "t^2 = 0") $
  if c /= 0 then      Some $ solve2 a b c
  else if b /= 0 then Some $ solve1 a b
  else if a /= 0 then Some $ Set.empty
  else All

intsct All All = All
intsct All (Some x) = Some x
intsct (Some x) All = Some x
intsct (Some x) (Some y) = Some (x `Set.intersection` y)

srt n = if n < 0 then Nothing else
        let a = squareRoot n
        in  if a^2 == n then Just a else Nothing

--   0 = (p - q) + (v - w)t + (a - b) t(1 + t)/2
--     = (p-q)   + ((v-w)+(a-b)/2)t  + (a-b)/2 . t^2
--     = 2(p-q)  + (2(v-w) + (a-b))t + (a-b) t^2
collide p q =
  -- trace ("colliding " ++ show p ++ " and " ++ show q) $
  let
      txs = crash x
      tys = crash y
      tzs = crash z
  in  -- trace ("have " ++ show txs ++ ", " ++ show tys ++ ", " ++ show tzs) $
      txs `intsct` tys `intsct` tzs
  where
    crash d = -- trace "doing one dimension" $
              solve (2 * (d (pos p) - d (pos q)))
                    (2 * (d (vel p) - d (vel q)) + (d (acc p) - d (acc q)))
                    (d (acc p) - d (acc q))


day20b ls =
  let parts = parse ls & zip [0..] & Map.fromList
      indexes = Map.keysSet parts
      surviving = removeCollisions indexes (earliestCollision parts)
  in Set.size surviving
  where
    positiveOnly (Some x) = Set.filter (>= 0) x
    earliestCollision parts =
      let cols = [(m, cs) | (m, p) <- Map.toList parts, (n, q) <- Map.toList parts,
                                          m /= n,
                                          let cs = positiveOnly (collide p q),
                                          cs /= Set.empty
                                          ]
               & Map.fromListWith Set.union
               & mapReverseAll
               & Map.toAscList
      in  trace ("found " ++ show (length cols) ++ " collisions") $
          cols
    removeCollisions keySet [] = keySet
    removeCollisions keySet ((_, collide):rest) =
      let reduced = collide `Set.intersection` keySet
      in  if Set.size reduced < 2 then removeCollisions keySet rest
          else removeCollisions (keySet `Set.difference` collide) rest
