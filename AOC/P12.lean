import Std.Data.HashMap

namespace AOC
namespace P12

-- format:
-- ????.######..#####. 1,6,5
-- ?#?#?#?#?#?#?#? 1,3,1,6
-- ?###???????? 3,2,1   -- lengde 12 -- 6stk '#' => 12-6 = 6stk '.'
-- #??##????###??????? 14,2

def countAlternatives (springs : List Char) (counts : List Nat) : Nat :=
  --dbg_trace s!"countAlternatives - {springs} - {counts}"
  match springs, counts with
  | [], [] =>
    --dbg_trace s!"countAlternatives - found one"
    1
  | l, [] => if l.all (λ it => it == '.' ∨ it == '?') then
      --dbg_trace s!"countAlternatives - found one"
      1
    else
      0
  | [], _ => 0
  | s::ss, c::cs =>
    if s == '.' then
      countAlternatives ss (c::cs)
    else if s == '?' then
      countAlternatives ('#'::ss) (c::cs) + countAlternatives ss (c::cs)
    else
      let sp := s::ss
      if sp.length < c then
        0
      else
        let firstC := sp.take c
        let hasEnough := firstC.length == c ∧ (firstC |>.all (λ it => it == '#'))
        let afterIsDotOrQuestion := sp.drop c |>.head? |>.map (λ it => it == '.' || it == '?') |>.getD true
        if hasEnough ∧ afterIsDotOrQuestion then
          countAlternatives (sp.drop (c+1)) cs
        else if hasEnough ∧ ¬afterIsDotOrQuestion then
          0
        else
          if sp.any (λ it => it == '?') then
            let b := sp.replace '?' '#'
            countAlternatives b (c::cs)
          else
            0
termination_by countAlternatives a _ => a
decreasing_by sorry

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ it => it.splitOn " ")

  let mut total := 0
  for it in s do
    let springs := it[0]!.data --++ ['.']
    let counts := it[1]!.splitOn "," |>.map (λ it => it.toNat!)
    IO.println s!"Trying to solve {springs} with {counts}"
    let cnt := countAlternatives springs counts
    IO.println s!"Found {cnt} alternatives"
    total := total + cnt
    --break
  IO.println s!"Total: {total}"



def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ it => it.splitOn " ")

  let mut total := 0
  for it in s do
    let springs := it[0]!.data
    let springs := springs ++ ['?'] ++ springs ++ ['?'] ++ springs ++ ['?'] ++ springs ++ ['?'] ++ springs
    let counts := it[1]!.splitOn "," |>.map (λ it => it.toNat!)
    let counts := counts ++ counts ++ counts ++ counts ++ counts
    IO.println s!"Trying to solve {springs} with {counts}"
    let cnt := 0
    IO.println s!"Found {cnt} alternatives"
    total := total + cnt
    --break
  IO.println s!"Total: {total}"

end P12
end AOC
