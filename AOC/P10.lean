namespace AOC
namespace P10

inductive Direction where
  | N
  | E
  | S
  | W

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

def directionToValidTiles (d : Direction) : Array Char :=
  match d with
  | Direction.N => #['|', 'F', '7']
  | Direction.E => #['-', '7', 'J']
  | Direction.S => #['|', 'L', 'J']
  | Direction.W => #['-', 'F', 'L']

-- using right hand rule to select neighbors
def directionAndTileToSideNeighbors (d : Direction) (t : Char) : Array Direction :=
  --dbg_trace s!"{d} {t}"
  match d, t with
  | Direction.N, '|' => #[Direction.E]
  | Direction.S, '|' => #[Direction.W]
  | Direction.E, '-' => #[Direction.S]
  | Direction.W, '-' => #[Direction.N]
  | Direction.W, 'F' => #[]
  | Direction.N, 'F' => #[Direction.N, Direction.W]
  | Direction.S, '7' => #[]
  | Direction.W, '7' => #[Direction.N, Direction.E]
  | Direction.E, 'J' => #[]
  | Direction.N, 'J' => #[Direction.S, Direction.E]
  | Direction.S, 'L' => #[]
  | Direction.E, 'L' => #[Direction.S, Direction.W]
  | _, _ => #[]

def directionToNextDirection (d : Direction) (t : Char) : Direction :=
  match d, t with
  | Direction.N, 'F' => Direction.E
  | Direction.N, '7' => Direction.W
  | Direction.N, _ => Direction.N
  | Direction.E, '7' => Direction.S
  | Direction.E, 'J' => Direction.N
  | Direction.E, _ => Direction.E
  | Direction.S, 'L' => Direction.E
  | Direction.S, 'J' => Direction.W
  | Direction.S, _ => Direction.S
  | Direction.W, 'F' => Direction.S
  | Direction.W, 'L' => Direction.N
  | Direction.W, _ => Direction.W

def findStart (s : Array (Array Char)) : (Nat × Nat) :=
  Id.run do
  let mut x := 0
  let mut y := 0
  for i in [0:s.size] do
    for j in [0:s[i]!.size] do
      if s[i]![j]! == 'S' then
        x := j
        y := i
  (x, y)

def navigate (map : Array (Array Char)) (start : (Nat × Nat)) (nextDirection : Direction) : IO Nat :=
do
  let maxX := map[0]!.size
  let maxY := map.size

  let mut pos := start
  let mut steps := 0
  let mut nextDirection := nextDirection
  while true do
    let (x, y) := pos

    let (dx, dy) := directionToCoord nextDirection
    let (x', y') := (x + dx, y + dy)
    --IO.println s!"{x} {y} {nextDirection} => {x'} {y'}"
    if x' < 0 || x' >= maxX || y' < 0 || y' >= maxY then
      return steps
    let x' := x'.toNat
    let y' := y'.toNat
    let t := map[y']![x']!
    --IO.println s!"Found a {t}"
    -- we are back at the start!
    if t == 'S' then
      return steps
    -- check if x' y' is a valid tile
    if !(directionToValidTiles nextDirection |>.contains t) then
      return steps

    steps := steps + 1
    nextDirection := directionToNextDirection nextDirection t
    --IO.println s!"Next direction is {nextDirection}"
    pos := (x', y')

  pure 1

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ it => it.data.toArray ) |>.toArray

  -- find starting point S
  let start := findStart s
  IO.print s!"{start}\n"

  IO.println "Trying to walk north"
  let d1 ← navigate s start Direction.N
  IO.println "Trying to walk east"
  let d2 ← navigate s start Direction.E
  IO.println "Trying to walk south"
  let d3 ← navigate s start Direction.S
  IO.println "Trying to walk west"
  let d4 ← navigate s start Direction.W

  IO.print s!"North: {d1}  East: {d2}  South: {d3} West:{d4}\n"
  let max := d1.max (d2.max (d3.max d4))
  IO.print s!"Max: {max}\n"
  -- halfway through the maze
  let halfway := (max.toFloat) / 2
  let halfway := halfway.ceil.toUInt32.toNat
  IO.print s!"Halfway: {halfway}\n"

inductive Tile where
  | Loop
  | Side
  | Unknown

instance : Inhabited Tile where
  default := Tile.Unknown

def color (map : Array (Array Tile)) (x : Nat) (y : Nat) (color : Tile) : Array (Array Tile) :=
  map.modify y (λ row => row.modify x (λ _ => color))

def colorSide (map : Array (Array Tile)) (x : Nat) (y : Nat) (tile : Char) (direction : Direction) : Array (Array Tile) :=
  directionAndTileToSideNeighbors direction tile |>.foldl (λ map direction =>
    let (dx, dy) := directionToCoord direction
    let (x', y') := (x + dx, y + dy)
    --dbg_trace s!"{x} {y}: {tile} dir:{direction} => {x'} {y'}"
    if x' < 0 || x' >= map[0]!.size || y' < 0 || y' >= map.size then
      map
    else
      map.modify y'.natAbs (λ row => row.modify x'.natAbs (λ existingTile => match existingTile with
        | Tile.Unknown => Tile.Side
        | _ => existingTile))
  ) map

def navigateAndColorMap (map : Array (Array Char)) (start : (Nat × Nat)) (nextDirection : Direction) : IO (Option (Array (Array Tile))) :=
do
  let maxX := map[0]!.size
  let maxY := map.size

  let mut coloredMap := map.map (λ it => it.map (λ _ => Tile.Unknown))

  let mut pos := start
  let mut steps := 0
  let mut nextDirection := nextDirection
  while true do
    let (x, y) := pos
    coloredMap := color coloredMap x y Tile.Loop
    coloredMap := colorSide coloredMap x y map[y]![x]! nextDirection

    let (dx, dy) := directionToCoord nextDirection
    let (x', y') := (x + dx, y + dy)
    --IO.println s!"{x} {y} {nextDirection} => {x'} {y'}"
    if x' < 0 || x' >= maxX || y' < 0 || y' >= maxY then
      return none
    let x' := x'.toNat
    let y' := y'.toNat
    let t := map[y']![x']!
    --IO.println s!"Found a {t}"
    -- we are back at the start!
    if t == 'S' then
      return coloredMap
    -- check if x' y' is a valid tile
    if !(directionToValidTiles nextDirection |>.contains t) then
      return none

    steps := steps + 1
    nextDirection := directionToNextDirection nextDirection t
    --IO.println s!"Next direction is {nextDirection}"
    pos := (x', y')

  pure none

def printMap (map : Option (Array (Array Tile))) : IO Unit :=
  match map with
  | none => IO.println "No map"
  | some map =>
    for row in map do
      for tile in row do
        match tile with
        | Tile.Loop => IO.print "#"
        | Tile.Side => IO.print "+"
        | Tile.Unknown => IO.print " "
      IO.println ""

def expand'' (map : Option (Array (Array Tile))) (x : Nat) (y : Nat) (dir : Direction) : Option (Array (Array Tile)) :=
  match map with
  | none => none
  | some map =>
    let (dx, dy) := directionToCoord dir
    let (x', y') := (x + dx, y + dy)
    if x' < 0 || x' >= map[0]!.size || y' < 0 || y' >= map.size then
      none
    else
      map.modify y'.natAbs (λ row => row.modify x'.natAbs (λ existingTile => match existingTile with
        | Tile.Unknown => Tile.Side
        | _ => existingTile))

def expand' (map : Option (Array (Array Tile))) (x : Nat) (y : Nat) : Option (Array (Array Tile)) :=
  let n :=  expand'' map x y Direction.N
  let e :=  expand'' n x y Direction.E
  let s :=  expand'' e x y Direction.S
  let w :=  expand'' s x y Direction.W
  w

def expand (map : Option (Array (Array Tile))) : IO (Option (Array (Array Tile))) :=
  match map with
  | none => pure none
  | some map => do
    let mut map := map
    for i in [0:map.size] do
      for j in [0:map[i]!.size] do
        match map[i]![j]! with
        | Tile.Side =>
          let x := expand' map j i
          match x with
          | none => return none
          | some x => map := x
        | _ => pure ()
    pure map

def count (map : Option (Array (Array Tile))) : IO Nat :=
  match map with
  | none => pure 0
  | some map =>
    do
    let mut count := 0
    for i in [0:map.size] do
      for j in [0:map[i]!.size] do
        match map[i]![j]! with
        | Tile.Side => count := count + 1
        | _ => pure ()
    pure count

def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ it => it.data.toArray ) |>.toArray

  -- find starting point S
  let start := findStart s
  IO.print s!"{start}\n"

  IO.println "Trying to walk north"
  let d1 ← navigateAndColorMap s start Direction.N
  let d1 ← expand d1
  let d1cnt ← count d1
  printMap d1
  IO.println "Trying to walk east"
  let d2 ← navigateAndColorMap s start Direction.E
  let d2 ← expand d2
  let d2cnt ← count d2
  printMap d2
  IO.println "Trying to walk south"
  let d3 ← navigateAndColorMap s start Direction.S
  let d3 ← expand d3
  let d3cnt ← count d3
  printMap d3
  IO.println "Trying to walk west"
  let d4 ← navigateAndColorMap s start Direction.W
  let d4 ← expand d4
  let d4cnt ← count d4
  printMap d4

  IO.print s!"North: {d1cnt}  East: {d2cnt}  South: {d3cnt} West:{d4cnt}\n"

end P10
end AOC
