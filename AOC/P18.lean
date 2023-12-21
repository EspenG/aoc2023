namespace AOC
namespace P18

inductive Direction
| Left
| Right
| Up
| Down

deriving instance Repr, Inhabited for Direction

instance : ToString Direction where
  toString d := match d with
  | Direction.Left => "Left"
  | Direction.Right => "Right"
  | Direction.Up => "Up"
  | Direction.Down => "Down"

namespace Direction

def toCoordDiff (d : Direction) : (Int × Int) :=
  match d with
  | Direction.Left => (-1, 0)
  | Direction.Right => (1, 0)
  | Direction.Up => (0, -1)
  | Direction.Down => (0, 1)

end Direction

structure Input :=
  (direction : Direction)
  (steps : Nat)
deriving instance Repr for Input

instance : ToString Input where
  toString i := s!"{i.direction} {i.steps}"

namespace Input

def parse (s : String) : Input :=
  let direction := match s.get 0 with
  | 'L' => Direction.Left
  | 'R' => Direction.Right
  | 'U' => Direction.Up
  | 'D' => Direction.Down
  | _ => panic! "invalid direction"
  let steps := s.drop 2 |>.takeWhile (λ c => c.isDigit) |>.toNat!
  ⟨direction, steps⟩

def hex2nat' (c : Char) : Nat :=
  if c.isDigit then c.toNat - '0'.toNat
  else if c.isLower then c.toNat - 'a'.toNat + 10
  else if c.isUpper then c.toNat - 'A'.toNat + 10
  else panic! "invalid hex digit"

def hex2nat (s : String) : Nat :=
  s.foldl (fun n c => n*16 + hex2nat' c) 0

def parse' (s : String) : Input :=
  -- format: R 6 (#70c710)
  let x := s.trimRight.splitOn " " |>.getLast! |>.drop 2 |>.dropRight 1
  let d := x.data.getLast!
  let d := match d with
  | '0' => Direction.Right
  | '1' => Direction.Down
  | '2' => Direction.Left
  | '3' => Direction.Up
  | _ => panic! "invalid direction"
  let c := x.dropRight 1
  ⟨ d, hex2nat c ⟩

end Input

def draw (grid : Array (Array Bool)) : IO Unit :=
do
  for row in grid do
    for cell in row do
      IO.print (if cell then "#" else ".")
    IO.println ""

def findMinMax (s : List Input) : IO ((Int × Int) × (Int × Int)) :=
do
  let mut maxX := 0
  let mut maxY := 0
  let mut minX := 0
  let mut minY := 0
  let mut x := 0
  let mut y := 0
  for move in s do
    let (dx, dy) := move.direction.toCoordDiff
    x := x + dx*move.steps
    y := y + dy*move.steps
    if x > maxX then maxX := x
    if y > maxY then maxY := y
    if x < minX then minX := x
    if y < minY then minY := y
  pure ((minX, minY), (maxX, maxY))

def set (grid : Array (Array Bool)) (point : Nat × Nat) :  Array (Array Bool) :=
  let (x, y) := point
  grid.set! y (grid.get! y |>.set! x true)

def expand' (grid : Array (Array Bool)) (point : Nat × Nat) (direction : Direction) : Array (Array Bool) :=
  let (x, y) := point
  -- if x,y is true we don't do anything
  if grid.get! y |>.get! x then grid
  else
    let g' := set grid (x, y)
    let directions := [Direction.Down, Direction.Left, Direction.Up, Direction.Right]
    directions.foldl (λ g d =>
      let c := d.toCoordDiff
      let newC : (Int × Int) := (x + c.fst, y + c.snd)
      let newC := (newC.fst.toNat, newC.snd.toNat)
      expand' g newC d) g'
termination_by expand' a _ _ => a
decreasing_by sorry

def expand (grid : Array (Array Bool)) : Array (Array Bool) :=
  -- we know there is a loop in the grid, but we don't know where it is
  -- all we know is that it touches all the edges of the grid at least once
  -- so we scan the second row for the first true, and then the first false after
  -- that and then expand the grid from there
  let onTheLoop := grid[1]!.findIdx? (λ b => b) |>.get!
  let firstInside := grid[1]!.toList |>.drop onTheLoop |>.toArray |>.findIdx? (λ b => !b) |>.get!
  let start := (onTheLoop+firstInside, 1)
  expand' grid start Direction.Right

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ s => Input.parse s)

  -- find max size
  let ((minX, minY),(maxX, maxY)) ← findMinMax s

  println! "minX: {minX}, minY: {minY}"
  println! "maxX: {maxX}, maxY: {maxY}"
  let mut grid := Array.mkArray (minY.natAbs + maxY.toNat + 1) (Array.mkArray (minX.natAbs + maxX.toNat + 1) false)

  let mut x : Int := minX.natAbs
  let mut y : Int := minY.natAbs
  for move in s do
    for _ in [0:move.steps] do
      let (dx, dy) := move.direction.toCoordDiff
      x := x + dx
      y := y + dy
      grid := grid.set! y.toNat (grid.get! y.toNat |>.set! x.toNat true)

  let grid' := expand grid
  --draw grid'

  -- count number of true cells
  let c := grid'.foldl (λ acc row => acc + row.foldl (λ acc cell => if cell then acc + 1 else acc) 0) 0
  IO.println c

def findCoords (startX startY : Int) (s : List Input) : IO (List (Int × Int)) :=
do
  let mut res := []
  let mut x := startX
  let mut y := startY
  for move in s do
    let (dx, dy) := move.direction.toCoordDiff
    x := x + dx*move.steps
    y := y + dy*move.steps
    res := (x, y) :: res
  pure res

def perimeter (s : List Input) : Int :=
  s.foldl (λ acc move => acc + move.steps) 0

def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ s => Input.parse' s)
  -- find max size
  let ((minX, minY),(maxX, maxY)) ← findMinMax s

  println! "minX: {minX}, minY: {minY}"
  println! "maxX: {maxX}, maxY: {maxY}"

  let perimiter := perimeter s

  let coordinates ← findCoords minX.natAbs minY.natAbs s

  -- zip coordinates with coordinates rotated 1 step
  let coordinates' := coordinates.drop 1 |>.append (coordinates.take 1)
  let coordinates := coordinates.zip coordinates'

  -- Shoelace formula
  let s : Int := coordinates.foldl (λ acc (c1, c2) =>
    let (x1, y1) := c1
    let (x2, y2) := c2
    let a := y1 + y2
    let b := x1 - x2
    acc + a*b) 0
  let s := s.natAbs / 2
  -- using Pick's theorem
  println! s+(perimiter/2)+1

end P18
end AOC
