import Std.Data.List.Basic

namespace AOC
namespace P13

def group_inputs (s: List String) : List (List String) :=
  let rec group_inputs' (s: List String) (acc: List (List String)) : List (List String) :=
    match s with
    | [] => acc
    | (h::t) =>
      if h == "" then
        group_inputs' t ([]::acc)
      else
        match acc with
        | [] => group_inputs' t [[h]]
        | (h'::t') => group_inputs' t ((h::h')::t')
  let x := group_inputs' s []
  x.reverse.map (λ it => it.reverse)

def printL (s: List (List Char)) : IO Unit :=
  for l in s do
    for c in l do
      IO.print c
    IO.println ""

def tryFindMirroring (s: List (List Char)) : IO (Option Nat) :=
  do
    let mut res := none
    for i in [1:s.length] do
      let left := s.take i |>.reverse
      let right := s.drop i
      if left.zip right |>.all (λ (a,b) => a == b) then
        res := some i
        --println! "found mirroring at {i}"
        --printL left.reverse
        --println! "---------"
        --printL right
        break
    pure res

-- we really only care about 3 cases:
-- 1. the two strings are the same
-- 2. the two strings have an edit distance of 1
-- 3. the two strings have an edit distance of 2 or more
def editDistance (s1 s2: List Char) : Nat :=
  s1.zip s2 |>.foldl (λ x (a,b) => x + if a == b then 0 else 1) 0

def tryFindFlawedMirroring (s: List (List Char)) : IO (Option Nat) :=
  do
    let mut res := none
    for i in [1:s.length] do
      let left := s.take i |>.reverse
      let right := s.drop i
      let ed := left.zip right |>.foldl (λ x (a,b) => if x <= 1 then x + editDistance a b else x) 0
      if ed == 1 then
        res := some i
        --println! "found mirroring at {i}"
        --printL left.reverse
        --println! "---------"
        --printL right
        break
    pure res

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |> group_inputs

  let mut total := 0
  for puzzle in s do
    --println! "NEW PUZZLE"
    let puzzle := puzzle.map (λ it => it.data )
    --printL puzzle
    --println! "+-+-+-+-+-+-+-+-"
    let res ← tryFindMirroring puzzle
    match res with
    | none =>
      --println! "did not find horizontal mirror, transposing"
      let t := puzzle.transpose
      let res ← tryFindMirroring t
      match res with
      | none =>
        IO.println "did not find mirror"
        printL puzzle
      | some x => total := total + x
    | some x => total := total + 100*x
  IO.println total

def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |> group_inputs

  let mut total := 0
  for puzzle in s do
    --println! "NEW PUZZLE"
    let puzzle := puzzle.map (λ it => it.data )
    --printL puzzle
    --println! "+-+-+-+-+-+-+-+-"
    let res ← tryFindFlawedMirroring puzzle
    match res with
    | none =>
      --println! "did not find horizontal mirror, transposing"
      let t := puzzle.transpose
      let res ← tryFindFlawedMirroring t
      match res with
      | none =>
        IO.println "did not find mirror"
        printL puzzle
      | some x => total := total + x
    | some x => total := total + 100*x
  IO.println total

end P13
end AOC
