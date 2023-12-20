import Std.Data.BinomialHeap.Basic
import Std.Data.HashMap

namespace AOC
namespace P17

inductive Direction
  | North (steps : Nat)
  | East (steps : Nat)
  | South (steps : Nat)
  | West (steps : Nat)

instance : ToString Direction where
  toString d := match d with
  | Direction.North c => s!"N{c}"
  | Direction.East c => s!"E{c}"
  | Direction.South c => s!"S{c}"
  | Direction.West c => s!"W{c}"

instance : Hashable Direction where
  hash d := match d with
  | Direction.North c => Hashable.hash 1 + Hashable.hash c
  | Direction.East c => Hashable.hash 2 + Hashable.hash c
  | Direction.South c => Hashable.hash 3 + Hashable.hash c
  | Direction.West c => Hashable.hash 4 + Hashable.hash c

instance : BEq Direction where
  beq a b := match a,b with
  | Direction.North x, Direction.North y => x==y
  | Direction.East x, Direction.East y => x==y
  | Direction.South x, Direction.South y => x==y
  | Direction.West x, Direction.West y => x==y
  | _, _ => false

def sameDir (a b : Direction) :=
  match a,b with
  | Direction.North _, Direction.North _ => true
  | Direction.East _, Direction.East _ => true
  | Direction.South _, Direction.South _ => true
  | Direction.West _, Direction.West _ => true
  | _, _ => false

def increment (d : Direction) : Direction :=
  match d with
  | Direction.North c => Direction.North (c+1)
  | Direction.East c => Direction.East (c+1)
  | Direction.South c => Direction.South (c+1)
  | Direction.West c => Direction.West (c+1)

def steps (d : Direction) : Nat :=
  match d with
  | Direction.North c => c
  | Direction.East c => c
  | Direction.South c => c
  | Direction.West c => c

structure Node :=
  (lowestCostSouth : Array Nat)
  (lowestCostEast : Array Nat)
  (lowestCostNorth : Array Nat)
  (lowestCostWest : Array Nat)

instance : ToString Node where
  toString n := s!"Node(S:{n.lowestCostSouth} E{n.lowestCostEast} N{n.lowestCostNorth} W{n.lowestCostWest})"

namespace Node

def init : Node :=
  { lowestCostSouth := Array.mkArray 11 10000000,
    lowestCostEast := Array.mkArray 11 10000000,
    lowestCostNorth := Array.mkArray 11 10000000,
    lowestCostWest := Array.mkArray 11 10000000 }

def zero : Node :=
  { lowestCostSouth := Array.mkArray 11 0,
    lowestCostEast := Array.mkArray 11 0,
    lowestCostNorth := Array.mkArray 11 0,
    lowestCostWest := Array.mkArray 11 0 }

def getLowestCost (n : Node) (d : Direction) : Nat :=
  match d with
  | Direction.North c => n.lowestCostNorth.getD (c-1) 10000000
  | Direction.East c => n.lowestCostEast.getD (c-1) 10000000
  | Direction.South c => n.lowestCostSouth.getD (c-1) 10000000
  | Direction.West c => n.lowestCostWest.getD (c-1) 10000000

def setLowestCost (n : Node) (d : Direction) (cost : Nat) : Node :=
  match d with
  | Direction.North c => { n with lowestCostNorth := n.lowestCostNorth.setD (c-1) cost }
  | Direction.East c => { n with lowestCostEast := n.lowestCostEast.setD (c-1) cost }
  | Direction.South c => { n with lowestCostSouth := n.lowestCostSouth.setD (c-1) cost }
  | Direction.West c => { n with lowestCostWest := n.lowestCostWest.setD (c-1) cost }

end Node

abbrev Point : Type := Nat × Nat

structure Vector :=
  (point : Point)
  (direction : Direction)

instance : Inhabited Vector := ⟨{ point := (0,0), direction := Direction.South 1 }⟩

instance : BEq Vector where
  beq a b := a.point == b.point && a.direction == b.direction

instance : Hashable Vector where
  hash v := (Hashable.hash v.point) + (Hashable.hash v.direction)

instance : ToString Vector where
  toString d := s!"p:{d.point} d:{d.direction}"

abbrev Graph : Type := Array (Array Nat)

abbrev PointGraph : Type := Std.HashMap Vector Vector
abbrev PointSet : Type := Std.HashMap Point Node

structure QueueItem :=
  (vector : Vector)
  (cost : Nat)

instance : Inhabited QueueItem := ⟨{ vector := { point := (0,0), direction := Direction.South 1 }, cost := 0 }⟩

namespace QueueItem

protected def le (a b : QueueItem) : Bool :=
  a.cost < b.cost

end QueueItem

abbrev Queue : Type := Std.BinomialHeap QueueItem QueueItem.le

def manhattanDistance (a b : Point) : Nat :=
  (((Int.ofNat a.fst) - b.fst)).natAbs + (((Int.ofNat a.snd) - b.snd)).natAbs

def dirToChar (d : Direction) : Char :=
  match d with
  | Direction.North _ => '^'
  | Direction.East _ => '>'
  | Direction.South _ => 'v'
  | Direction.West _ => '<'

def draw (g : Graph) (pg : PointGraph) (p : Vector) : IO Unit :=
do
  -- convert g to char array
  let mut gg := g.map (fun row => row.map (fun c => c.digitChar))

  let mut p := p
  while p.point != (0,0) do
    gg := gg.setD p.point.snd (gg[p.point.snd]!.setD p.point.fst (dirToChar p.direction))
    --dbg_trace "looking for {p}"
    p := pg.find! p
  --gg := gg.setD 0 (gg[0]!.setD 0 'X')

  for y in [0:gg.size] do
    for x in [0:gg[0]!.size] do
      IO.print gg[y]![x]!
    println! ""

def directionToCoord (d : Direction) : (Int × Int) :=
  match d with
  | Direction.North _ => (0, -1)
  | Direction.East _ => (1, 0)
  | Direction.South _ => (0, 1)
  | Direction.West _ => (-1, 0)

def isOppositeDirection (a b : Direction) : Bool :=
  match a,b with
  | Direction.North _, Direction.South _ => true
  | Direction.East _, Direction.West _ => true
  | Direction.South _, Direction.North _ => true
  | Direction.West _, Direction.East _ => true
  | _, _ => false

def walk (g : Graph) (existing : Vector) (dir : Direction) : Option Vector :=
  let (dx,dy) := directionToCoord dir
  let (x,y) := existing.point
  let (x,y) := (x + dx, y + dy)
  if x < 0 || x >= g[0]!.size || y < 0 || y >= g.size then
    none
  else
    let dir := if sameDir existing.direction dir then increment existing.direction else dir
    -- can't walk more than 3 steps in the same direction
    if steps dir > 3 || isOppositeDirection existing.direction dir then
      none
    else
      some { point := (x.natAbs,y.natAbs), direction := dir }

def a_star (walk : Graph → Vector → Direction → Option Vector) (isDone : Vector → Point → Bool) (g : Graph) (start stop : Point) : Option (PointGraph × Vector) :=
do
  let mut openSet : Queue := Std.BinomialHeap.empty
    |>.insert { vector := { point := start, direction := Direction.South 0 }, cost := 0 }

  let mut cameFrom : PointGraph := Std.HashMap.empty
  let mut gScore : PointSet := Std.HashMap.empty.insert start Node.zero
  let mut toReturn := none

  while !openSet.isEmpty do
    let currentAndNewQ := openSet.deleteMin |>.get!
    openSet := currentAndNewQ.snd
    let current := currentAndNewQ.fst
    --dbg_trace "current: {current.vector}"
    if isDone current.vector stop then
      openSet := Std.BinomialHeap.empty
      toReturn := some (cameFrom, current.vector)
      continue
      --return some (cameFrom, current.vector)

    let currentNode := gScore.findD current.vector.point Node.init
    let costOfWalkingToCurentNode := currentNode.getLowestCost current.vector.direction

    for dir in [Direction.North 1, Direction.East 1, Direction.South 1, Direction.West 1] do
      let neighbor := walk g current.vector dir
      match neighbor with
      | none => continue
      | some neighbor =>
        -- tentative_gScore := gScore[current] + d(current, neighbor)
        let tentative_gScore := costOfWalkingToCurentNode + g[neighbor.point.snd]![neighbor.point.fst]!
        let old := gScore.findD neighbor.point Node.init
        let oldBest := old.getLowestCost neighbor.direction
        if tentative_gScore < oldBest then
          -- This path to neighbor is better than any previous one. Record it!
          cameFrom := cameFrom.insert neighbor current.vector
          gScore := gScore.insert neighbor.point (old.setLowestCost neighbor.direction tentative_gScore)
          let qItem := { vector := neighbor, cost := tentative_gScore + manhattanDistance neighbor.point stop }
          openSet := openSet.insert qItem

  toReturn

def isDone (v : Vector) (p : Point) : Bool :=
  v.point == p

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n"
    |>.dropLast
    |>.map (fun f => f.data.map (fun c => c.toNat - '0'.toNat) |>.toArray)
    |>.toArray

  let x := a_star walk isDone s (0,0) (s[0]!.size-1,s.size-1)
  let mut cost := 0
  match x with
  | none => println! "no path found"
  | some (x, p) =>
    --draw s x p
    let mut p := p
    while p.point != (0,0) do
      cost := cost + s[p.point.snd]![p.point.fst]!
      p := x.find! p
      dbg_trace "cost: {s[p.point.snd]![p.point.fst]!}"
  println! "heat loss: {cost}"

def isDone' (v : Vector) (p : Point) : Bool :=
  v.point == p && steps v.direction > 4

def walk' (g : Graph) (existing : Vector) (dir : Direction) : Option Vector :=
  let (dx,dy) := directionToCoord dir
  let (x,y) := existing.point
  let (x,y) := (x + dx, y + dy)
  if x < 0 || x >= g[0]!.size || y < 0 || y >= g.size then
    none
  else
    let oldCount := steps existing.direction
    if oldCount == 0 then -- special case for start
      some { point := (x.natAbs,y.natAbs), direction := dir }
    else
      let isSameDir := sameDir existing.direction dir
      let dir := if isSameDir then increment existing.direction else dir
      -- make a special case so that we can walk in the beginning
      if steps dir > 10 || isOppositeDirection existing.direction dir || (!isSameDir && oldCount < 4) then
        none
      else
        some { point := (x.natAbs,y.natAbs), direction := dir }

def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n"
    |>.dropLast
    |>.map (fun f => f.data.map (fun c => c.toNat - '0'.toNat) |>.toArray)
    |>.toArray

  let x := a_star walk' isDone' s (0,0) (s[0]!.size-1,s.size-1)
  let mut cost := 0
  match x with
  | none => println! "no path found"
  | some (x, p) =>
    draw s x p
    let mut p := p
    while p.point != (0,0) do
      cost := cost + s[p.point.snd]![p.point.fst]!
      p := x.find! p
      dbg_trace "cost: {s[p.point.snd]![p.point.fst]!}"
  println! "heat loss: {cost}"


end P17
end AOC
