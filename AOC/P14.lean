import Std.Data.List.Basic
import Std.Data.HashMap
namespace AOC
namespace P14

def moveOneStep (s : Array (List Char)) (frm to : Nat) : Array (List Char) :=
  let a := s.get! frm
  let b := s.get! to
  let (a,b) := a.zip b |>.map (λ it => match it with
  | ('O', '.') => ('.', 'O')
  | (x, y) => (x, y)
  ) |>.unzip
  s.set! frm a |>.set! to b

def tiltNorth (s : List (List Char)) : IO (List (List Char)) :=
do
  let mut s := s.toArray
  for i in [1:s.size+1] do
    for j in [1:i] do
      let k := i - j
      --println! "Moving rocks from row {k} to {k-1}"
      s := moveOneStep s k (k-1)
  pure s.toList

def tiltSouth (s : List (List Char)) : IO (List (List Char)) :=
do
  let mut s := s.toArray
  for i in [1:s.size+1] do
    for j in [0:i] do
      let j' := i-j
      let k' := s.size - j'
      s := moveOneStep s (k'-1) k'
  pure s.toList

def tiltWest (s : List (List Char)) : IO (List (List Char)) :=
do
  let x := s.transpose
  let x ← tiltNorth x
  pure x.transpose

def tiltEast (s : List (List Char)) : IO (List (List Char)) :=
do
  let x := s.transpose
  let x ← tiltSouth x
  pure x.transpose

def calculateWeight (s : List (List Char)) : Int :=
  let w := s.length
  s.foldlIdx (λ idx acc it =>
    let bla := it.filter (fun it => it == 'O') |>.length
    acc + (bla*(w-idx))) 0

def printBoard (s: List (List Char)) : IO Unit :=
  for l in s do
    for c in l do
      IO.print c
    IO.println ""

def loopStep (s : List (List Char)) : IO (List (List Char)) :=
do
  let s ← tiltNorth s
  let s ← tiltWest s
  let s ← tiltSouth s
  let s ← tiltEast s
  pure s

def hash (s : List (List Char)) : UInt64 :=
  s.foldl (λ acc it => acc ++ it.toString) "" |>.hash

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ it => it.data)

  let board ← tiltNorth s
  --printBoard board
  let w := calculateWeight board
  println! "{w}"

def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let mut board := f.splitOn "\n" |>.dropLast |>.map (λ it => it.data)

  let mut boards : Std.HashMap UInt64 Nat := Std.HashMap.empty

  for i in [0:1000000000] do
    board ← loopStep board
    let h := hash board
    --println! "{i} {h}"
    match boards.find? h with
    | none => boards := boards.insert h i
    | some bi =>
      println! "Found a loop at {i}"
      let diff := i - bi
      println! "Loops every {diff} boards"
      let remaining := 1000000000 - i
      println! "Remaining {remaining} iterations"
      let remaining := remaining % diff
      println! "mod {diff} => {remaining}"
      for i in [1:remaining] do
        board ← loopStep board
      let w := calculateWeight board
      println! "{w}"
      return ()

end P14
end AOC
