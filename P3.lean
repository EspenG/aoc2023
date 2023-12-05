import Std.Data.RBMap.Basic

def has_symbol (s: String) : Bool :=
  s.any (fun c => !c.isDigit && c != '.')

def p3a : IO Unit :=
do
  let empty := "............"
  let f ← IO.FS.readFile "P3/input.txt"
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

-- define RBMap for StarLocations (x,y) -> count
--def StarLocations := Std.RBMap Lean.Position Nat Lean.Position.lt

-- def p3b : IO Unit :=
-- do
--   let empty := "............"
--   let f ← IO.FS.readFile "P3/testinput1.txt"
--   let s := f.splitOn "\n" |>.dropLast |>.map (fun line => "." ++ line ++ ".")
--   let s := empty :: s ++ [empty]
--   let mut total := 0
--   let mut stars StarLocations := Std.RBMap.empty

--   for i in [1:s.length-1] do
--     let line := s.get! i
--     --IO.print s!"=> {line}\n"
--     let mut bla := line.toSubstring
--     --IO.print s!"   => {bla}\n"
--     let mut x := 1

--     while x <= line.length-1 do
--       let sub := bla.drop x
--       let digitStart := sub.dropWhile (fun c => !c.isDigit)
--       let digit := digitStart.takeWhile (fun c => c.isDigit)
--       if !digit.isEmpty then
--         --println! "digit: {digit}"
--         let prev_line := s.get! (i-1) |>.drop (digit.startPos.byteIdx-1) |>.take (digit.bsize+2)
--         let prev_has_star := has_star prev_line |>.isSome
--         let next_line := s.get! (i+1) |>.drop (digit.startPos.byteIdx-1) |>.take (digit.bsize+2)
--         let next_has_star := has_star next_line |>.isSome
--         let before := line.drop (digit.startPos.byteIdx-1) |>.take 1
--         let before_has_star := has_star before |>.isSome
--         let after := line.drop (digit.stopPos.byteIdx) |>.take 1
--         let after_has_star := has_star after |>.isSome
--         let sym := before_has_star || after_has_star || prev_has_star || next_has_star
--         if sym then
--           total := total + digit.toNat?.get!
--         --println! "digit: {digit} has_sym: {sym} before: {before} after: {after} prev: {prev_line} next: {next_line}"
--       x := digit.stopPos.byteIdx + 1

--   IO.print s!"{total}\n"
