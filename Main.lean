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
    ("4a", AOC.P4.a),   ("4b", AOC.P4.b),
    ("5a", AOC.P5.a),   ("5b", AOC.P5.b),
    ("6a", AOC.P6.a),   ("6b", AOC.P6.b),
    ("7a", AOC.P7.a),   ("7b", AOC.P7b.b),
    ("8a", AOC.P8.a),   ("8b", AOC.P8.b),
    ("9a", AOC.P9.a),   ("9b", AOC.P9.b),
    ("10a", AOC.P10.a),   ("10b", AOC.P10.b),
    ("11a", AOC.P11.a),   ("11b", AOC.P11.b),
    ("12a", AOC.P12.a),   ("12b", AOC.P12.b),
    ("13a", AOC.P13.a),   ("13b", AOC.P13.b),
    ("14a", AOC.P14.a),   ("14b", AOC.P14.b),
    ("15a", AOC.P15.a),   ("15b", AOC.P15.b),
    ("16a", AOC.P16.a),   ("16b", AOC.P16.b),
    ("17a", AOC.P17.a),   ("17b", AOC.P17.b),
    ("18a", AOC.P18.a),   ("18b", AOC.P18.b),
    ("19a", AOC.P19.a),   ("19b", AOC.P19.b),
    ("20a", AOC.P20.a),   ("20b", AOC.P20.b),
    ("21a", AOC.P21.a),   ("21b", AOC.P21.b),
    ("22a", AOC.P22.a),   ("22b", AOC.P22.b),
    ("23a", AOC.P23.a),   ("23b", AOC.P23.b),
    ("24a", AOC.P24.a),   ("24b", AOC.P24.b),
    ("25a", AOC.P25.a),   ("25b", AOC.P25.b)
  ]
  let methodToCall := methods.find? (fun (m : String × (String → IO Unit)) => m.1 == method)
  match methodToCall with
  | none => throw (IO.userError "Method not found")
  | some m => do
    m.2 inputFileName
