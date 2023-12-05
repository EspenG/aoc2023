import Std.Data.RBMap.Basic

def Bla := Std.RBMap String Nat Ord.compare

def p2a : IO Unit :=
do
  let f ← IO.FS.readFile "P2/input.txt"
  let s := f.splitOn "\n" |>.dropLast

  let mut total := 0

  let limits : Bla := Std.RBMap.empty
  |>.insert "red" 12
  |>.insert "green" 13
  |>.insert "blue" 14

  for line in s do
    let (game,rest) := match line.splitOn ":" with
      | [a, b] => (a, b)
      | _ => panic! "Unexpected format 1"

    let id := game.splitOn " " |>.get! 1 |>.toNat!
    let sets := rest.splitOn ";"
    let mut allSetsOk := true
    for set in sets do
      let displayed := set.trim.splitOn ", "
         |>.map (fun x => match x.splitOn " " with
        | [a, b] => (a.toInt!, b)
        | _ => panic! "Unexpected format 2"
      )
      let allOk := displayed.all (fun (n, c) => match limits.find? c with
        | some l => n <= l
        | none => false
      )
      if !allOk then
        allSetsOk := false
        break
    if allSetsOk then
      total := total + id
      --println! "{id} is ok"


  IO.print s!"{total}\n"

def p2b : IO Unit :=
do
  let f ← IO.FS.readFile "P2/input.txt"
  let s := f.splitOn "\n" |>.dropLast

  let mut total := 0

  for line in s do
    let (game,rest) := match line.splitOn ":" with
      | [a, b] => (a, b)
      | _ => panic! "Unexpected format 1"

    let id := game.splitOn " " |>.get! 1 |>.toNat!
    let sets := rest.splitOn ";"
    let mut minValues : Bla := Std.RBMap.empty
    for set in sets do
      let displayed := set.trim.splitOn ", "
         |>.map (fun x => match x.splitOn " " with
        | [a, b] => (a.toNat!, b)
        | _ => panic! "Unexpected format 2"
      )
      for (n, c) in displayed do
        match minValues.find? c with
        | some m => minValues := minValues.insert c (Nat.max m n)
        | none => minValues := minValues.insert c n
    let power := minValues.foldl (fun acc _ x => acc * x) 1
    total := total + power
    --IO.println s!"id {id}: {minValues.toList} -> {power}"

  IO.print s!"total: {total}\n"
