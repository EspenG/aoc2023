namespace AOC
namespace P5

-- format of input:

-- seeds: 79 14 55 13

-- seed-to-soil map:
-- 50 98 2
-- 52 50 48

-- soil-to-fertilizer map:
-- 0 15 37
-- 37 52 2
-- 39 0 15

-- fertilizer-to-water map:
-- 49 53 8

-- etc..

structure MapingRange where
  destination : Nat
  source : Nat
  length : Nat
deriving Repr

def MapingRange.contains (m : MapingRange) (n : Nat) : Bool :=
  m.source <= n && n < m.source + m.length

def MapingRange.containsRange (m : MapingRange) (n : MapingRange) : Bool :=
  m.source <= n.source &&
  n.source + n.length <= m.source + m.length

-- returns the part of m that is less than n
def MapingRange.mkLess (m : MapingRange) (n : MapingRange) : MapingRange :=
  MapingRange.mk m.destination m.source (n.source - m.source)

-- returns the part of m that is more than n
def MapingRange.mkMore (m : MapingRange) (n : MapingRange) : MapingRange :=
  let n_end := n.source + n.length
  let skip := n_end - m.source
  MapingRange.mk (m.destination + skip) (m.source + skip) (m.length - skip)

instance : ToString MapingRange :=
⟨λ m => "{" ++ "destination: " ++ toString m.destination ++ ", " ++
         "source: " ++ toString m.source ++ ", " ++
         "length: " ++ toString m.length ++ "}"⟩

instance : LT MapingRange where
  lt m n := m.source < n.source

instance : DecidableRel (LT.lt : MapingRange → MapingRange → Prop) :=
λ m n => if m.source < n.source then Decidable.isTrue sorry else Decidable.isFalse sorry

inductive MappingNode where
  | nil
  | node (less : MappingNode) (more : MappingNode) (range : MapingRange)

def MappingNode.mkRoot : MappingNode :=
  MappingNode.node MappingNode.nil MappingNode.nil (MapingRange.mk 0 0 100000000000000)

def MappingNode.rangeMapsTo (n : MappingNode) (i : Nat) (cnt : Nat) : Prod Nat Nat :=
  match n with
  | MappingNode.nil => Prod.mk 0 0
  | MappingNode.node less more range =>
    if range.contains i then
      Prod.mk (range.destination + (i - range.source)) (min cnt (range.length - (i - range.source)))
    else
      if i < range.source then
        less.rangeMapsTo i cnt
      else
        more.rangeMapsTo i cnt

def MappingNode.mapsTo (n : MappingNode) (i : Nat) : Nat :=
  match n with
  | MappingNode.nil => i
  | MappingNode.node less more range =>
    if range.contains i then
      range.destination + (i - range.source)
    else
      if i < range.source then
        less.mapsTo i
      else
        more.mapsTo i

def MappingNode.range (n : MappingNode) : MapingRange :=
  match n with
  | MappingNode.nil => MapingRange.mk 0 0 0
  | MappingNode.node _ _ r => r

def MappingNode.insert (n : MappingNode) (r : MapingRange) : MappingNode :=
  match n with
  | MappingNode.nil => MappingNode.node MappingNode.nil MappingNode.nil r
  | MappingNode.node less more range =>
    if n.range |>.containsRange r then
      -- we split the range into 3 parts
      -- 1. the parts of n that are less than r
      -- 2. r
      -- 3. the parts of n that are more than r
      -- 1 or 3 can be empty
      -- we insert 1 and 3 into the tree and make r the new root

      -- make a node of the less part
      let lessRange := range.mkLess r
      let lessNode := if lessRange.length == 0 then
        less
      else
        MappingNode.node less MappingNode.nil lessRange

      -- make a node of the more part
      let moreRange := range.mkMore r
      let moreNode := if moreRange.length == 0 then
        more
      else
        MappingNode.node MappingNode.nil more moreRange

      -- make r the new root
      MappingNode.node lessNode moreNode r
    else
      -- walk down the tree
      if r < n.range then
        MappingNode.node (less.insert r) more range
      else
        MappingNode.node less (more.insert r) range

instance : Inhabited MappingNode :=
⟨MappingNode.nil⟩

def print_mapping_tree (n: MappingNode) : IO Unit :=
  let rec print_mapping_tree' (n: MappingNode) (indent: String) : IO Unit :=
    match n with
    | MappingNode.nil => pure ()
    | MappingNode.node less more range =>
    do
      IO.println s!"{indent}{range}"
      print_mapping_tree' less (indent ++ "  ")
      print_mapping_tree' more (indent ++ "  ")
  print_mapping_tree' n ""

instance : ToString MappingNode where
  toString n :=
    match n with
    | MappingNode.nil => "nil"
    | MappingNode.node _ _ range =>
      "node(?, ?, " ++ toString range ++ ")"

def print_mapping_range (n: MappingNode) : IO Unit :=
  for i in [0:100] do
    IO.println s!"{i} -> {n.mapsTo i}"

def parse_map (s: List String) : List MapingRange :=
  s.dropLast
    |>.map (λ s => s.splitOn " "
    |>.map String.toNat!)
    |>.map (λ | [a, b, c] => MapingRange.mk a b c
              | _ => MapingRange.mk 0 0 0)



def parse_maps (s: List String) : List (List String) :=
  -- lets use recursion, because why not
  -- we split into groups by empty lines
  let rec parse_maps' (s: List String) (acc: List (List String)) : List (List String) :=
    match s with
    | [] => acc
    | (h::t) =>
      if h == "" then
        parse_maps' t ([]::acc)
      else
        match acc with
        | [] => parse_maps' t [[h]]
        | (h'::t') => parse_maps' t ((h::h')::t')
  parse_maps' s []

def create_mapping_tree (s: List MapingRange) : MappingNode :=
  s.foldl (λ acc r => acc.insert r) MappingNode.mkRoot

-- maps a list to a list of pairs
def pairs {α : Type} (l : List α) : List (α × α) :=
  let rec pairs' {α : Type} (l : List α) (acc : List (α × α)) : List (α × α) :=
    match l with
    | [] => acc
    | [_] => acc
    | (a::b::t) => pairs' t ((a, b)::acc)
  pairs' l []

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast

  let seeds := s.get! 0 |>.splitOn ":" |>.get! 1 |>.trim |>.splitOn " " |>.map String.toNat!

  let s := s.drop 2
  let g := parse_maps s |> List.map parse_map |> List.map create_mapping_tree |>.reverse

  let mut lowest := 100000000000000
  for seed in seeds do
    let mapped := g.foldl (λ acc n => n.mapsTo acc) seed
    lowest := min lowest mapped
  IO.println s!"lowest: {lowest}"

def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast

  let seeds := s.get! 0 |>.splitOn ":" |>.get! 1 |>.trim |>.splitOn " " |>.map String.toNat! |> pairs

  let s := s.drop 2
  let g := parse_maps s |> List.map parse_map |> List.map create_mapping_tree |>.reverse

  let mut lowest := 100000000000000
  for (seed, cnt) in seeds do
    let mut cnt := cnt
    let mut seed := seed
    while cnt > 0 do
      let (mapped, checked) := g.foldl (λ acc n => n.rangeMapsTo acc.fst acc.snd)
        (Prod.mk seed cnt)
      lowest := min lowest mapped
      cnt := cnt - checked
      seed := seed + checked
  IO.println s!"lowest: {lowest}"

end P5
end AOC
