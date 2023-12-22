import Lean.Data.Parsec
import Std.Data.HashMap

open Lean Parsec

namespace AOC
namespace P19

-- format is like this:
-- name{var>val,}
-- hdj{m>838:A,pv}
--
-- {x=787,m=2655,a=1222,s=2876}

inductive Operator
| gt
| lt

abbrev WorkflowName := String

inductive Goto
| workflow (name : WorkflowName)
| Rejected
| Accepted

structure Rule where
  var : String
  operator : Operator
  val : Nat
  goto: Goto

inductive WorkflowItem where
  | goto (g : Goto)
  | rule (c : Rule)

structure Workflow where
  name : WorkflowName
  conditions : Array WorkflowItem

structure Part where
  x : Nat
  m : Nat
  a : Nat
  s : Nat

structure Input where
  workflows : Array Workflow
  parts : Array Part

def comma : Parsec Char := pchar ','
def colon : Parsec Char := pchar ':'
def lbrace : Parsec Char := pchar '{'
def rbrace : Parsec Char := pchar '}'
def equals : Parsec Char := pchar '='
def gt : Parsec Char := pchar '>'
def lt : Parsec Char := pchar '<'
def newline : Parsec String := pstring "\n"

def lowercase : Parsec Char := satisfy fun c =>
  0x61 ≤ c.val ∧ c.val ≤ 0x7a

def number : Parsec String := many1Chars (satisfy Char.isDigit)

def identifier : Parsec String := many1Chars lowercase

def gotoHelper : Parsec Goto :=
  do
    let name ← identifier
    return Goto.workflow name

def goto : Parsec Goto :=
  do
    let c ← peek!
    match c with
    | 'A' =>
      let _ ← pstring "A"
      return Goto.Accepted
    | 'R' =>
      let _ ← pstring "R"
      return Goto.Rejected
    | _ => gotoHelper

def rule : Parsec Rule :=
  do
    let var ← identifier
    let op ← (gt *> return Operator.gt) <|> (lt *> return Operator.lt)
    let val ← number
    let _ ← colon
    let g ← goto
    return { var := var, operator := op, val := val.toNat!, goto := g }

def manySep (p : Parsec α) (s : Parsec β) : Parsec $ Array α := do
  manyCore (attempt (s *> p)) #[←p]

def workflowItemRule : Parsec WorkflowItem :=
  do
    let r ← rule
    return WorkflowItem.rule r

def workflowItemGoto : Parsec WorkflowItem :=
  do
    let g ← goto
    return WorkflowItem.goto g

def workflowItem : Parsec WorkflowItem :=
  -- goto or rule
  do
    let g ← workflowItemRule-- <|> workflowItemGoto
    --let _ ← comma
    return g

def workflow : Parsec Workflow :=
  do
    let name ← identifier
    let _ ← lbrace
    let conditions ← manySep workflowItem comma
    let _ ← comma
    let last ← workflowItemGoto
    let _ ← rbrace
    let _ ← newline
    return { name := name, conditions := conditions ++ #[last] }

def workflows : Parsec $ Array Workflow :=
  manySep workflow newline

def part : Parsec Part :=
  -- format looks like this:
  -- {x=787,m=2655,a=1222,s=2876}
  do
    let _ ← lbrace
    let _ ← pstring "x="
    let x ← number
    let _ ← comma
    let _ ← pstring "m="
    let m ← number
    let _ ← comma
    let _ ← pstring "a="
    let a ← number
    let _ ← comma
    let _ ← pstring "s="
    let s ← number
    let _ ← rbrace
    let _ ← newline
    return { x := x.toNat!, m := m.toNat!, a := a.toNat!, s := s.toNat! }

def parts : Parsec $ Array Part :=
  many part

def file : Parsec $ Input :=
  do
    let workflows ← many1 workflow
    let _ ← newline
    let parts ← many1 part
    let _ ← (optional newline)
    let _ ← eof
    return { workflows := workflows, parts := parts }

def parse (s : String) : Except String $ Input :=
  match file s.mkIterator with
  | Parsec.ParseResult.success _ res => Except.ok res
  | Parsec.ParseResult.error it err  => Except.error s!"offset {it.i}: {err} -- {it}"

def getVar (var : String) (p : Part) : Nat :=
  match var with
  | "x" => p.x
  | "m" => p.m
  | "a" => p.a
  | "s" => p.s
  | _ => panic! "unknown var"

def createHandler (w : Workflow) : Part → Goto :=
  (fun x =>
    let r := w.conditions.foldl (fun acc c => match acc with
      | some b => b
      | none => match c with
        | WorkflowItem.goto g => g
        | WorkflowItem.rule r =>
          let op : Int → Int → Bool := match r.operator with
          | Operator.gt => (fun x y => x > y)
          | Operator.lt => (fun x y => x < y)

          if op (getVar r.var x) r.val then
            r.goto
          else
            none
      ) none
    match r with
    | some b => b
    | none => Goto.Rejected)

def process' (wf : HashMap String (Part → Goto)) (p : Part) (state : String) : Bool :=
  let w := wf.find? state
  match w with
  | none => false
  | some w =>
    match w p with
    | Goto.Accepted => true
    | Goto.Rejected => false
    | Goto.workflow name => process' wf p name
termination_by process' a _ _ => a
decreasing_by { sorry }

def process (wf : HashMap String (Part → Goto)) (p : Part) : Bool :=
  process' wf p "in"

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let i := parse f
  match i with
  | Except.ok i =>
    let hm := HashMap.empty
    let hm := i.workflows.foldl (fun acc w => acc.insert w.name (createHandler w)) hm
    let x := i.parts.filter (fun p => process hm p) |>.foldl (fun acc p => acc + p.x + p.m + p.a + p.s) 0
    IO.println x
  | Except.error e => IO.println e

structure Range where
  min : Nat
  max : Nat

instance : Inhabited Range := ⟨{ min := 0, max := 0 }⟩

namespace Range

def length (r : Range) : Nat :=
  r.max - r.min + 1

def split (r : Range) (val : Nat) : Range × Range :=
  ({ min := r.min, max := val - 1 }, { min := val + 1, max := r.max })


end Range

structure PartRange where
  x : Range
  m : Range
  a : Range
  s : Range

instance : Inhabited PartRange := ⟨{
  x := Inhabited.default,
  m := Inhabited.default,
  a := Inhabited.default,
  s := Inhabited.default }⟩

namespace PartRange

def numCombinations (r : PartRange) : Nat :=
  r.x.length * r.m.length * r.a.length * r.s.length

def getRange (r : PartRange) (var : String) : Range :=
  match var with
  | "x" => r.x
  | "m" => r.m
  | "a" => r.a
  | "s" => r.s
  | _ => panic! "unknown var"

def setRange (r : PartRange) (var : String) (a : Range) : PartRange :=
  match var with
  | "x" => { r with x := a }
  | "m" => { r with m := a }
  | "a" => { r with a := a }
  | "s" => { r with s := a }
  | _ => panic! "unknown var"

end PartRange

instance : Inhabited PartRange := ⟨{
  x := { min := 1, max := 4000 },
  m := { min := 1, max := 4000 },
  a := { min := 1, max := 4000 },
  s := { min := 1, max := 4000 } }⟩

def ruleHandler (rule : Rule) (range : PartRange) : PartRange × (Goto × PartRange) :=
  let r := range.getRange rule.var
  let (a,b) := r.split rule.val
  -- rule.val is not in either a or b, we need to add it back to the correct one
  -- based on rule.operator
  let (a,b) := match rule.operator with
  | Operator.gt => (Range.mk a.min (a.max + 1), b)
  | Operator.lt => (a, Range.mk (b.min - 1) b.max)
  let a := range.setRange rule.var a
  let b := range.setRange rule.var b

  match rule.operator with
  | Operator.gt => (a, (rule.goto, b))
  | Operator.lt => (b, (rule.goto, a))

def createRangeHandler (w : Workflow) : PartRange → List (Goto × PartRange) :=
  fun wholeRange =>
    let (_, r) := w.conditions.foldl (fun (range, partialResult) cond =>
      match cond with
        | WorkflowItem.goto g => (range, (g, range) :: partialResult)
        | WorkflowItem.rule r =>
          let (leftoverRange, pair) := ruleHandler r range
          (leftoverRange, pair  :: partialResult)
    ) (wholeRange, [])
    r

def countCombinations (wf : HashMap String (PartRange → List (Goto × PartRange))) (state : String) (range : PartRange) : Nat :=
  let w := wf.find? state
  match w with
  | none => 0
  | some workflowFn =>
    workflowFn range |>.foldl (fun acc (g, r) => match g with
      | Goto.Accepted => acc + r.numCombinations
      | Goto.Rejected => acc
      | Goto.workflow name => acc + countCombinations wf name r
    ) 0
termination_by countCombinations a _ _ => a
decreasing_by { sorry }

def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let i := parse f
  match i with
  | Except.ok i =>
    let hm := HashMap.empty
    let hm := i.workflows.foldl (fun acc w => acc.insert w.name (createRangeHandler w)) hm
    let x := countCombinations hm "in" Inhabited.default
    IO.println x
  | Except.error e => IO.println e


end P19
end AOC
