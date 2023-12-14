namespace AOC
namespace P9

def find_next_num' (series : List Int) : Int :=
  let x := series.zip (series.drop 1) |>.map (fun (x, y) => (y-x))
  -- check if all elements are the same
  if x.all (fun it => it == 0) then
    0
  else
    find_next_num' x + x.getLast!
termination_by find_next_num' b => b
decreasing_by { sorry }

def find_next_num (series : List Int) : IO Int :=
do
  let x := find_next_num' series + series.getLast!
  pure x

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (fun x => x.splitOn " " |>.map String.toInt!)

  let mut total := 0
  for series in s do
    let next <- find_next_num series
    total := total + next
    IO.println s!"{series} -> {next}"
  IO.print s!"{total}\n"

def find_prev_num' (series : List Int) : Int :=
  let x := series.zip (series.drop 1) |>.map (fun (x, y) => (y-x))
  -- check if all elements are the same
  if x.all (fun it => it == 0) then
    0
  else
    x.head! - find_prev_num' x
termination_by find_prev_num' b => b
decreasing_by { sorry }

def find_prev_num (series : List Int) : IO Int :=
do
  let x := series.head! - find_prev_num' series
  pure x

def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (fun x => x.splitOn " " |>.map String.toInt!)

  let mut total := 0
  for series in s do
    let next <- find_prev_num series
    total := total + next
    IO.println s!"{series} -> {next}"
  IO.print s!"{total}\n"

end P9
end AOC
