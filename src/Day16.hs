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
                                 in trace ((pad 20 $ show m) ++ "\t" ++ finish s') $
                                    s') state moves

finish (seq, part) = let back = part & Map.toList & map swap & Map.fromList in
                     toList seq & map (back Map.!)

day16 ls =
  let init = initState ['a'..'p']
      prog = parse ls
  in trace ("starting state:" ++ finish init)
     finish $ run init prog

{-
-}

day16b ls =
  let init = initState ['a'..'p']
      prog = parse ls
  in finish $ run init prog
