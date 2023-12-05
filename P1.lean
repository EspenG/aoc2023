import Lean.Data.Trie

def reverse (s : String) : String :=
  ⟨s.data.reverse⟩

def takeFirstNumber (s : String) : String :=
  s.dropWhile Char.isAlpha |>.takeWhile Char.isDigit |>.front |>.toString

def p1a : IO Unit :=
do
  let f ← IO.FS.readFile "P1/input.txt"
  let s := f.splitOn "\n"

  let total := s.foldl (fun x y =>
    let a := takeFirstNumber y
    let b := takeFirstNumber (reverse y)

    let cal := a ++ b |>.toNat?

    match cal with
    | none => x
    | some cal => x + cal
  ) 0

  IO.print s!"{total}\n"


def NumberTrie := Lean.Data.Trie String

def createNumberTrie : NumberTrie :=
  let emptyTrie : NumberTrie := Lean.Data.Trie.empty
  emptyTrie
  |>.insert "one" "1"
  |>.insert "two" "2"
  |>.insert "three" "3"
  |>.insert "four" "4"
  |>.insert "five" "5"
  |>.insert "six" "6"
  |>.insert "seven" "7"
  |>.insert "eight" "8"
  |>.insert "nine" "9"


def find_first_num_front (s : String) (numberTrie : NumberTrie) : Option String :=
  if s.isEmpty then
    none
  else
    if s.front.isDigit then
      some s.front.toString
    else
    match numberTrie.find? (s.take 3) with
    | none => match numberTrie.find? (s.take 4) with
      | none => match numberTrie.find? (s.take 5) with
        | none => find_first_num_front (s.drop 1) numberTrie
        | some n => some n
      | some n => some n
    | some n => some n
  termination_by find_first_num_front s _ => s
  decreasing_by { simp_wf; sorry }

def find_first_num_back (s : String) (numberTrie : NumberTrie) : Option String :=
  if s.isEmpty then
    none
  else
    if s.back.isDigit then
      some s.back.toString
    else
    match numberTrie.find? (s.takeRight 3) with
    | none => match numberTrie.find? (s.takeRight 4) with
      | none => match numberTrie.find? (s.takeRight 5) with
        | none => find_first_num_back (s.dropRight 1) numberTrie
        | some n => some n
      | some n => some n
    | some n => some n
  termination_by find_first_num_back s _ => s
  decreasing_by { simp_wf; sorry }


def p1b : IO Unit :=
do
  let f ← IO.FS.readFile "P1/input.txt"
  let s := f.splitOn "\n" |>.dropLast

  let numberTrie := createNumberTrie
  -- map of numbers as strings to numbers
  -- let spelledNumbers :=

  -- for y in s do
  --   IO.print s!"line: {y}\n"
  --   match find_first_num_front y numberTrie with
  --   | none => IO.print s!"-- {y}\n"
  --   | some n => IO.print s!"-- {n}\n"

    -- let spelledOutToNumbers := y.foldl (fun x y =>
    --   let a := x ++ y.toString
    --   match numberTrie.find? a with
    --   | none => x
    --   | some n => x ++ n
    -- ) ""

    --IO.print s!"{spelledOutToNumbers}\n"

  let total := s.foldl (fun x y =>

    let a := find_first_num_front y numberTrie |>.get!
    let b := find_first_num_back y numberTrie |>.get!

    let cal := a ++ b |>.toNat?

    match cal with
    | none => x
    | some cal => x + cal
  ) 0

  IO.print s!"{total}\n"
