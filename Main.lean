import AOC

def main (args : List String) : IO Unit :=
do
  if args.length != 2 then
    throw (IO.userError "Usage: <method> <inputFileName>")
  let method := args.get! 0
  let inputFileName := args.get! 1
  IO.println s!"Running the program... {method} with {inputFileName}";

  let methods := [
    ("1a", AOC.P1.p1a), ("1b", AOC.P1.p1b),
    ("2a", AOC.P2.p2a), ("2b", AOC.P2.p2b),
    ("3a", AOC.P3.p3a), ("3b", AOC.P3.p3b),
    ("4a", AOC.P4.a),   ("4b", AOC.P4.b)
  ]
  let methodToCall := methods.find? (fun (m : String × (String → IO Unit)) => m.1 == method)
  match methodToCall with
  | none => throw (IO.userError "Method not found")
  | some m => do
    m.2 inputFileName
