import Std.Data.Array.Init.Basic

namespace AOC
namespace P16


inductive Direction where
  | N
  | E
  | S
  | W

instance : BEq Direction where
  beq d1 d2 := match d1, d2 with
  | Direction.N, Direction.N => true
  | Direction.E, Direction.E => true
  | Direction.S, Direction.S => true
  | Direction.W, Direction.W => true
  | _, _ => false

instance : ToString Direction where
  toString d := match d with
  | Direction.N => "N"
  | Direction.E => "E"
  | Direction.S => "S"
  | Direction.W => "W"

def directionToCoord (d : Direction) : (Int × Int) :=
  match d with
  | Direction.N => (0, -1)
  | Direction.E => (1, 0)
  | Direction.S => (0, 1)
  | Direction.W => (-1, 0)

def nextDirection (d : Direction) (t : Char) : List Direction :=
  match d, t with
  | Direction.W, '|' => [Direction.N, Direction.S]
  | Direction.E, '|' => [Direction.N, Direction.S]
  | Direction.N, '-' => [Direction.W, Direction.E]
  | Direction.S, '-' => [Direction.W, Direction.E]
  | Direction.W, '\\' => [Direction.N]
  | Direction.E, '\\' => [Direction.S]
  | Direction.N, '\\' => [Direction.W]
  | Direction.S, '\\' => [Direction.E]
  | Direction.W, '/' => [Direction.S]
  | Direction.E, '/' => [Direction.N]
  | Direction.N, '/' => [Direction.E]
  | Direction.S, '/' => [Direction.W]
  | d, _ => [d]


def walk (grid : Array (Array Char)) (x : Int) (y : Int) (d : Direction) (currentPath : Array (Array (List Direction))) : Array (Array (List Direction)) :=

  let next_dir := nextDirection d (grid.get! y.toNat |>.get! x.toNat)

  let rec walk' (a : Array (Array (List Direction))) (d : Direction) : Array (Array (List Direction)) :=
    let (dx, dy) := directionToCoord d
    let (x, y) := (x + dx, y + dy)
    if x < 0 || y < 0 || y >= grid.size || x >= (grid.get! 0 |>.size) then
      a
    else
      -- check that we have not entered a loop, we do that by looking up the traveling direction
      -- in location x,y in currentPath and if it is contained in the list we have a loop
      let listDirs := a.get! y.toNat |>.get! x.toNat
      if listDirs.contains d then
        a
      else
        let currentPath' := a.set! y.toNat (a.get! y.toNat |>.set! x.toNat (d :: listDirs))
        walk grid x y d currentPath'

  next_dir.foldl walk' currentPath

termination_by
  walk' a _ => a
  walk _ _ _ _ a => a
decreasing_by { sorry }

def numEnergized (grid : Array (Array (List Direction))) : Int :=
  grid.foldl (fun acc row => acc + row.foldl (fun acc' c => if !c.isEmpty then acc' + 1 else acc') 0) 0

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (fun x => x.data.toArray) |>.toArray

  let e := Array.mkArray s.size (Array.mkArray s[0]!.size [])
  let e := e.set! 0 (e.get! 0 |>.set! 0 [Direction.E])

  let res := walk s 0 0 Direction.E e
  let tot := numEnergized res
  println! "Result: {tot}"


def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (fun x => x.data.toArray) |>.toArray

  let empty := Array.mkArray s.size (Array.mkArray s[0]!.size [])
  let mut max := 0

  -- go east and west
  for i in [0:s.size] do
    -- east
    let e := empty.set! i (empty.get! 0 |>.set! 0 [Direction.E])
    let res := walk s 0 i Direction.E e |> numEnergized
    if res > max then
      max := res
    -- west
    let maxE := (empty.get! 0 |>.size) - 1
    let e := empty.set! i (empty.get! maxE |>.set! maxE [Direction.W])
    let res := walk s maxE i Direction.W e |> numEnergized
    if res > max then
      max := res
  -- go north and south
  for i in [0:s[0]!.size] do
    -- south
    let e := empty.set! 0 (empty.get! i |>.set! i [Direction.S])
    let res := walk s i 0 Direction.S e |> numEnergized
    if res > max then
      max := res
    -- north
    let maxS := (empty.size) - 1
    let e := empty.set! maxS (empty.get! i |>.set! i [Direction.N])
    let res := walk s i maxS Direction.N e |> numEnergized
    if res > max then
      max := res
  println! "Result: {max}"

end P16
end AOC
