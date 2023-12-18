import Std.Data.List.Basic

namespace AOC
namespace P15

def hash (s : String) : Nat :=
  s.foldl (λ acc c => (acc + c.toNat) * 17 % 256) 0

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.get! 0 |>.splitOn "," |>.map (λ it => hash it )

  let s := s.foldl (λ acc c => acc + c) 0
  println! s


def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.get! 0 |>.splitOn ","

  let map := Array.mkArray 256 []

  let map := s.foldl (λ acc c =>
    if c.endsWith "-" then
      -- remove from map
      let key := c.dropRight 1
      let h := hash key
      let l := acc.get! h |>.filter (λ it => it.fst != key)
      acc.set! h l
    else
      -- add to map or replace
    match c.splitOn "=" with
    | [key, value] =>
      let h := hash key
      let l := acc.get! h
      if l.any (λ it => it.fst == key) then
        acc.set! h (l.map (λ it => if it.fst == key then (key, value) else it))
      else
        acc.set! h (l.append [(key, value)])
    | _ => panic! "invalid input"
  ) map

  let mut res := 0
  for i in [0:256] do
    let l := map.get! i
    let x := l.mapIdx (λ idx it =>
      (i+1) * (idx + 1) * it.snd.toNat!
    ) |>.foldl (λ acc c => acc + c) 0
    res := res + x
  println! res

end P15
end AOC
