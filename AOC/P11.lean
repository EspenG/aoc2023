import Std.Data.Array.Init.Basic
import Std.Data.List.Basic

namespace AOC
namespace P11

def makePairs {α : Type} (xs : List α) : List (α × α) :=
  let rec loop (xs : List α) (acc : List (α × α)) : List (α × α) :=
    match xs with
    | [] => acc
    | x::xs => loop xs (acc ++ xs.map (λ it => (x, it)))
  loop xs []

def manhattanDistance (a : (Nat × Nat)) (b : (Nat × Nat)) : Int :=
  (((Int.ofNat a.fst) - b.fst)).natAbs + (((Int.ofNat a.snd) - b.snd)).natAbs

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.toArray

  -- check if there are rows with only '.' if so double it
  let s := s.map (λ it => if it.all (λ it => it == '.') then #[it, it] else #[it]) |>.flatten

  -- transpose columns to rows
  let s := s.toList |>.map (λ it => it.toList) |>.transpose

  -- check again if there are any rows with only '.' if so double it
  let s := s.map (λ it => if it.all (λ it => it == '.') then #[it, it] else #[it]) |>.toArray |>.flatten |>.toList |>.transpose

  -- find coordinates of all #
  let coords := s.mapIdx (λ y row => row.mapIdx (λ x it => if it == '#' then some (x, y) else none))
  -- filter out all none values
  let coords := coords.map (λ it => it.filterMap (λ x => x) |>.toArray) |>.toArray |>.flatten |>.toList

  -- create pairs of all coordinates [a,b,c] to [(a,b), (a,c), (b,c)]
  let pairs := makePairs coords

  -- calculate the manhattan distance between all pairs
  let distances := pairs.map (λ it => manhattanDistance it.fst it.snd)

  let total := distances.foldl (λ acc it => acc + it) 0

  IO.println total

def numInSpecialRange (lst : List Nat) (a : Nat) (b : Nat) : Nat :=
  let lower := a.min b
  let upper := a.max b
  lst.filter (λ it => it > lower && it < upper) |>.length

def specialManhattanDistance (x : List Nat) (y : List Nat) (a : (Nat × Nat)) (b : (Nat × Nat)) : Int :=
  let factor := 1000000
  let numX := numInSpecialRange x a.fst b.fst
  let numY := numInSpecialRange y a.snd b.snd
  let xdir := (((Int.ofNat a.fst) - b.fst)).natAbs
  let xdir := xdir - numX + numX*factor
  let ydir := (((Int.ofNat a.snd) - b.snd)).natAbs
  let ydir := ydir - numY + numY*factor
  xdir + ydir


def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ it => it.toList)

  -- collect y axis with all empty space
  let y := s.mapIdx (λ row it => if it.all (λ it => it == '.') then some row else none) |>.filterMap (λ x => x)
  IO.println s!"index of empty rows: {y}"

  -- transpose columns to rows
  let transposed := s.transpose

  -- collect x axis with all empty space
  let x := transposed.mapIdx (λ row it => if it.all (λ it => it == '.') then some row else none) |>.filterMap (λ x => x)
  IO.println s!"index of empty cols: {x}"

  -- find coordinates of all #
  let coords := s.mapIdx (λ y row => row.mapIdx (λ x it => if it == '#' then some (x, y) else none))
  -- filter out all none values
  let coords := coords.map (λ it => it.filterMap (λ x => x) |>.toArray) |>.toArray |>.flatten |>.toList

  -- create pairs of all coordinates [a,b,c] to [(a,b), (a,c), (b,c)]
  let pairs := makePairs coords

  -- calculate the manhattan distance between all pairs
  let distances := pairs.map (λ it => specialManhattanDistance x y it.fst it.snd)

  let total := distances.foldl (λ acc it => acc + it) 0

  IO.println total


end P11
end AOC
