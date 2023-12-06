import Std.Data.RBMap.Basic

namespace AOC
namespace P3

def has_symbol (s: String) : Bool :=
  s.any (fun c => !c.isDigit && c != '.')

def p3a (filename : String) : IO Unit :=
do
  let empty := "............"
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (fun line => "." ++ line ++ ".")
  let s := empty :: s ++ [empty]
  let mut total := 0

  for i in [1:s.length-1] do
    let line := s.get! i
    --IO.print s!"=> {line}\n"
    let mut bla := line.toSubstring
    --IO.print s!"   => {bla}\n"
    let mut x := 1

    while x <= line.length-1 do
      let sub := bla.drop x
      let digitStart := sub.dropWhile (fun c => !c.isDigit)
      let digit := digitStart.takeWhile (fun c => c.isDigit)
      if !digit.isEmpty then
        --println! "digit: {digit}"
        let prev_line := s.get! (i-1) |>.drop (digit.startPos.byteIdx-1) |>.take (digit.bsize+2)
        let next_line := s.get! (i+1) |>.drop (digit.startPos.byteIdx-1) |>.take (digit.bsize+2)
        let before := line.drop (digit.startPos.byteIdx-1) |>.take 1
        let after := line.drop (digit.stopPos.byteIdx) |>.take 1
        let sym := has_symbol before || has_symbol after || has_symbol prev_line || has_symbol next_line
        if sym then
          total := total + digit.toNat?.get!
        --println! "digit: {digit} has_sym: {sym} before: {before} after: {after} prev: {prev_line} next: {next_line}"
      x := digit.stopPos.byteIdx + 1

  IO.print s!"{total}\n"

def has_star (s: String) : Option Nat :=
  if s.any (fun c => c == '*') then
  do {
    -- find index of * and return it
    let mut i := 0
    while i < s.length do
      if s.get! (String.Pos.mk i) == '*' then
        return i
      i := i + 1
    none
  }
  else
    none

instance : Ord (Nat × Nat) where
  compare a b := if a.1 = b.1 then compare a.2 b.2 else compare a.1 b.1

-- define RBMap for StarLocations (x,y) -> count
def StarLocations := Std.RBMap (Nat × Nat) Nat compare

def create_star_location (x: Nat) (y: Nat) : (Nat × Nat) :=
  (x,y)

def p3b (filename : String) : IO Unit :=
do


  let empty := "............"
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (fun line => "." ++ line ++ ".")
  let s := empty :: s ++ [empty]
  let mut total := 0

  let mut stars : StarLocations := Std.RBMap.empty

  for i in [1:s.length-1] do
    let line := s.get! i
    --IO.print s!"=> {line}\n"
    let mut bla := line.toSubstring
    --IO.print s!"   => {bla}\n"
    let mut x := 1

    while x <= line.length-1 do
      let sub := bla.drop x
      let digitStart := sub.dropWhile (fun c => !c.isDigit)
      let digit := digitStart.takeWhile (fun c => c.isDigit)
      if !digit.isEmpty then
        let n := digit.toNat?.get!
        --println! "digit: {digit}"
        let prev_line := s.get! (i-1)
          |>.drop (digit.startPos.byteIdx-1)
          |>.take (digit.bsize+2)
          |> has_star
          |>.map (fun x => create_star_location (x+digit.startPos.byteIdx-1) (i-1))
        let next_line := s.get! (i+1)
          |>.drop (digit.startPos.byteIdx-1)
          |>.take (digit.bsize+2)
          |> has_star
          |>.map (fun x => create_star_location (x+digit.startPos.byteIdx-1) (i+1))
        let before := line.drop (digit.startPos.byteIdx-1)
          |>.take 1
          |> has_star
          |>.map (fun x => create_star_location (x+digit.startPos.byteIdx-1) i)
        let after := line.drop (digit.stopPos.byteIdx)
          |>.take 1
          |> has_star
          |>.map (fun x => create_star_location (x+digit.stopPos.byteIdx) i)
        -- we know that only one of the previous 4 can be a star so lets compine all the options into 1
        let star := [prev_line, next_line, before, after].foldl (fun acc x => acc.orElse (λ _ => x)) none
        match star with
        | some star =>
          --IO.print s!"star at {star} {n}\n"
          let existingUser := stars.find? star
          match existingUser with
          | some other_n => total := total + other_n * n
          | none => stars := stars.insert star n
        | none => IO.lazyPure (fun _ => ())

      x := digit.stopPos.byteIdx + 1

  IO.print s!"{total}\n"

end P3
end AOC
