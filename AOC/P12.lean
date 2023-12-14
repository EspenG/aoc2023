namespace AOC
namespace P12

-- format:  ????.######..#####. 1,6,5

def violatesConstraints (fixedConstraints : List Nat) (currentGrouping : List Nat) : Bool :=
  if fixedConstraints.length < currentGrouping.length then
    -- hard to gauge, but lets make sure the counts make sense at least
    fixedConstraints.foldl (λ acc it => acc + it) 0 <= currentGrouping.foldl (λ acc it => acc + it) 0
  else if fixedConstraints.length == currentGrouping.length then
    fixedConstraints.zip currentGrouping |>.any (λ it => it.1 < it.2)
  else
    -- we have to compare in order, but we can skip fixedConstraints.length - currentGrouping.length items anywhere in the list
    Id.run do
      let fixedConstraints := fixedConstraints.toArray
      let mut availableToSkip := fixedConstraints.size - currentGrouping.length
      let mut fixedIdx := 0
      for it in currentGrouping do
        if it <= fixedConstraints[fixedIdx]! then
          fixedIdx := fixedIdx + 1
        else
          if availableToSkip == 0 then
            return true
          availableToSkip := availableToSkip - 1
          fixedIdx := fixedIdx + 1
      return false


def calculateGrouping' (springs : List Char) (currentCount : Nat) (res : List Nat) : List Nat :=
  if springs.isEmpty then
    if currentCount > 0 then res ++ [currentCount] else res
  else
    if springs[0]! == '#' then
      calculateGrouping' (springs.drop 1) (currentCount + 1) res
    else
      if currentCount > 0 then
        calculateGrouping' (springs.drop 1) 0 (res ++ [currentCount])
      else
        calculateGrouping' (springs.drop 1) 0 res
termination_by calculateGrouping' a _ _ => a
decreasing_by { sorry }

-- count number of consecutive # into a list
-- #????.######..#####.  -> [1,6,5]
def calculateGrouping (springs : List Char) : List Nat :=
  calculateGrouping' springs 0 []

--#eval calculateGrouping "#????.######..#####.".data

-- replace the first instance of ? with c
def replaceFirst (springs : List Char) (c : Char) : (List Char × List Nat) :=
  let r := springs.replace '?' c
  let g := calculateGrouping r
  (r, g)

def matchesConstraint (springs : List Char) (fixedConstraints : List Nat) : Bool :=
  let actualGrouping := calculateGrouping springs
  actualGrouping == fixedConstraints

def countAlternatives' (springs : List Char) (fixedConstraints : List Nat) (currentGrouping : List Nat) : Nat :=
  dbg_trace s!"{springs} {fixedConstraints} {currentGrouping}"
  if violatesConstraints fixedConstraints currentGrouping then 0
  else
    dbg_trace "Trying a replacement"
    if springs.all (λ it => it != '?') then
      if matchesConstraint springs fixedConstraints then 1 else 0
    else
      let (springs', currentGrouping') := replaceFirst springs '.'
      let (springs'', currentGrouping'') := replaceFirst springs '#'
      countAlternatives' springs' fixedConstraints currentGrouping' + countAlternatives' springs'' fixedConstraints currentGrouping''
termination_by countAlternatives' a _ _ => a
decreasing_by { sorry }

def countAlternatives (springs : List Char) (counts : List Nat) : Nat :=
  countAlternatives' springs counts (calculateGrouping springs)


def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ it => it.splitOn " ")

  let mut total := 0
  for it in s do
    let springs := it[0]!.data
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
  let s := f.splitOn "\n" |>.dropLast |>.map (λ it => it.data.toArray ) |>.toArray

end P12
end AOC
