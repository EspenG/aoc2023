namespace AOC
namespace P14

def tiltNorth (s : List String) : List String :=
  s

def calculateWeight (s : List String) : Int :=
  0

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast

  let board := tiltNorth s
  let w := calculateWeight board
  println! "{w}"


def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ it => it.data.toArray ) |>.toArray

end P14
end AOC
