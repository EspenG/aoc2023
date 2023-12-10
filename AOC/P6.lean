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

def upperRecord' (min : Nat) (max : Nat) (timeout : Nat) (toBeat : Nat) : Nat :=
  let guessFn := fun a b => (a + b) / 2
  if min >= max then
    max
  else
    let currentGuess := guessFn min max
    let d := race currentGuess timeout
    if d < toBeat then
      upperRecord' min (currentGuess - 1) timeout toBeat
    else
      upperRecord' (currentGuess + 1) max timeout toBeat
  termination_by upperRecord' a _ _ _ => a
  decreasing_by { sorry }

def upperRecord (timeout : Nat) (toBeat : Nat) (lower : Nat) : Nat :=
  upperRecord' lower timeout timeout toBeat

def lowerRecord' (min : Nat) (max : Nat) (timeout : Nat) (toBeat : Nat) : Nat :=
  let guessFn := fun a b => (a + b) / 2
  if min == max then
    min + 1
  else
    let currentGuess := guessFn min max
    let d := race currentGuess timeout
    if d > toBeat then
      lowerRecord' min (currentGuess - 1) timeout toBeat
    else
      lowerRecord' (currentGuess + 1) max timeout toBeat
  termination_by lowerRecord' a _ _ _ => a
  decreasing_by { sorry }

def lowerRecord (timeout : Nat) (toBeat : Nat) (upper : Nat) : Nat :=
  lowerRecord' 1 upper timeout toBeat

def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast

  let time := s.get! 0 |>.splitOn ":" |>.get! 1 |>.replace " " "" |>.toNat!
  let record := s.get! 1 |>.splitOn ":" |>.get! 1 |>.replace " " "" |>.toNat!

  let firstGuess := time / 2
  let d := race firstGuess time
  if d < record then
    println! "Failed to find a starting point!"
    return ()

  let lower := lowerRecord time record firstGuess
  let upper := upperRecord time record firstGuess

  let diff := upper - lower + 2
  println! "{lower} - {upper} = {diff}"

end P6
end AOC
