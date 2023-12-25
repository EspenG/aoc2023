namespace AOC
namespace P22

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast



def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast


end P22
end AOC
