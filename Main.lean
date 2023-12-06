import AOC

def main (args : List String) : IO Unit :=
do
  if args.length != 2 then
    throw (IO.userError "Usage: <method> <inputFileName>")
  let method := args.get! 0
  let inputFileName := args.get! 1
  IO.println s!"Running the program... {method} with {inputFileName}";

  let methods := [
    ("p1a", AOC.P1.p1a), ("p1b", AOC.P1.p1b),
    ("p2a", AOC.P2.p2a), ("p2b", AOC.P2.p2b),
    ("p3a", AOC.P3.p3a), ("p3b", AOC.P3.p3b),
    ("p4a", AOC.P4.p4a)--, ("p4b", AOC.P4.p4b),
  ]
  let methodToCall := methods.find? (fun (m : String × (String → IO Unit)) => m.1 == method)
  match methodToCall with
  | none => throw (IO.userError "Method not found")
  | some m => do
    m.2 inputFileName
