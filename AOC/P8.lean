import Std.Data.HashMap
import Std.Data.Nat.Gcd

namespace AOC
namespace P8

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast

  let instructions := s.get! 0
  let s := s.drop 2

  let graph := s.map (λ line =>
    let parts := line.splitOn " "
    let key := parts.get! 0
    let left := (parts.get! 2).drop 1 |>.dropRight 1
    let right := (parts.get! 3).dropRight 1
    (key, (left, right))
  )

  let graph := graph.foldl (λ g (key, v) => g.insert key v) Std.HashMap.empty

  let mut nextKey := "AAA"
  let mut steps := 0
  let mut done := false

  for _ in [0:200000] do
    for c in instructions.data do
      steps := steps + 1
      let mnk := graph.find? nextKey |>.map (λ (left, right) =>
        if c == 'L' then
          left
        else
          right
      )
      match mnk with
      | none => IO.println "none"
      | some nk => nextKey := nk
      if nextKey == "ZZZ" then
        IO.println s!"Done! {steps}"
        done := true
        break
    if done then
      break

def find_loop (graph : Std.HashMap String (String × String)) (insts : String) (key : String) : IO Nat :=
do
  let mut nextKey := key
  let mut steps := 0
  let mut done := false
  let mut firstZ := ""
  let mut firstZSteps := 0

  -- walk until we find the first __z
  for _ in [0:20000000000] do
    for c in insts.data do
      steps := steps + 1
      let mnk := graph.find? nextKey |>.map (λ (left, right) =>
        if c == 'L' then
          left
        else
          right
      )
      match mnk with
      | none => pure ()
      | some nk => nextKey := nk
      -- if firstZ != "" && nextKey == firstZ then
      --   done := true
      --   IO.println s!"Found loop {firstZ} at {steps-firstZSteps}"
      --   break
      if firstZ == "" && nextKey.drop 2 == "Z" then
        firstZ := nextKey
        firstZSteps := steps
        --IO.println s!"Found first Z {firstZ} at {firstZSteps}"
        done := true
        break

    if done then
      break

  pure steps

def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast

  let instructions := s.get! 0
  let s := s.drop 2

  let graph := s.map (λ line =>
    let parts := line.splitOn " "
    let key := parts.get! 0
    let left := (parts.get! 2).drop 1 |>.dropRight 1
    let right := (parts.get! 3).dropRight 1
    (key, (left, right))
  )

  let mut state := graph.filter (λ (key, _) => key.drop 2 == "A") |>.map (λ (key, _) => key)
  let graph := graph.foldl (λ g (key, v) => g.insert key v) Std.HashMap.empty

  IO.println state

  let mut x := []

  for it in state do
    let s <- find_loop graph instructions it
    x := x ++ [s]

  let lcm := x.foldl (λ a b => Nat.lcm a b) 1
  IO.println s!"First common divisor is {lcm}"

end P8
end AOC
