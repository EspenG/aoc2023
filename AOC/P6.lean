namespace AOC
namespace P6

def race (timeToCharge : Nat) (timeout : Nat) : Nat :=
  let timeToRace := timeout - timeToCharge
  timeToRace * timeToCharge

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast

  let time := s.get! 0 |>.splitOn ":" |>.get! 1 |>.splitOn " " |>.map String.trim |>.filter (· ≠ "") |>.map String.toNat!
  let record := s.get! 1 |>.splitOn ":" |>.get! 1 |>.splitOn " " |>.map String.trim |>.filter (· ≠ "") |>.map String.toNat!

  let mut tot := 1
  for (time, record) in time.zip record do
    let mut numRecords := 0
    for i in [0:time] do
      let dist := race i time
      if dist > record then
        numRecords := numRecords + 1
    tot := tot * numRecords
  println! "{tot}"

def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast

end P6
end AOC
