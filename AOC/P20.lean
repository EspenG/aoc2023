import Std.Data.HashMap
import Std.Data.Nat.Gcd
namespace AOC
namespace P20

inductive Puls
  | Low
  | High

instance : ToString Puls := ⟨λ p => match p with
  | Puls.Low => "Low"
  | Puls.High => "High"⟩

instance : BEq Puls := ⟨λ a b => match a,b with
  | Puls.Low, Puls.Low => true
  | Puls.High, Puls.High => true
  | _, _ => false
⟩

inductive FlipFlopState
  | Off
  | On

instance : ToString FlipFlopState := ⟨λ s => match s with
  | FlipFlopState.Off => "Off"
  | FlipFlopState.On => "On"⟩

inductive RxState
  | NotDone
  | Done

inductive Gate
 | Conjunction (state : Std.HashMap String Puls)
 | FlipFlop (state : FlipFlopState)
 | Broadcaster
 | Rx (state : RxState)
 | Untyped

instance : Inhabited Gate := ⟨Gate.Untyped⟩

instance : ToString Gate := ⟨λ g => match g with
  | Gate.Conjunction x => s!"Conjunction({x.toList})"
  | Gate.FlipFlop x => s!"FlipFlop({x})"
  | Gate.Broadcaster => "Broadcaster"
  | Gate.Rx x => "Rx({x})"
  | Gate.Untyped => "Untyped"⟩

def extractType (s : String) : Gate × String :=
  if s == "broadcaster" then
    (Gate.Broadcaster, "broadcaster")
  else
    match s.data.head! with
    | '%' => (Gate.FlipFlop FlipFlopState.Off, s.drop 1)
    | '&' => (Gate.Conjunction Std.HashMap.empty, s.drop 1)
    | _ => (Gate.Untyped, s)

abbrev State := Std.HashMap String (Gate × List String)

def processSignal (state : State) : IO (State × Nat × Nat) :=
do
  let mut state := state
  let mut cntLow := 0
  let mut cntHigh := 0
  let mut q := Std.Queue.empty.enqueue ("button", "broadcaster", Puls.Low)
  while ¬q.isEmpty do
    match q.dequeue? with
    | none => panic! "empty queue"
    | some ((sender, receiver, pulse), q') =>
        if pulse == Puls.Low then cntLow := cntLow + 1 else cntHigh := cntHigh + 1
        q := q'
        match state.find? receiver with
        | none => pure () --panic! s!"unknown receiver {receiver}"
        | some (gate, nextReceivers) =>
          match gate with
          | Gate.Broadcaster =>
            for nextReceiver in nextReceivers do
              q := q.enqueue (receiver, nextReceiver, pulse)
          | Gate.FlipFlop flipFlopState =>
            if pulse == Puls.Low then
              let (nextState, nextSignal) :=  match flipFlopState with
                | FlipFlopState.Off => (FlipFlopState.On, Puls.High)
                | FlipFlopState.On => (FlipFlopState.Off, Puls.Low)
              for nextReceiver in nextReceivers do
                q := q.enqueue (receiver, nextReceiver, nextSignal)
              state := state.insert receiver (Gate.FlipFlop nextState, nextReceivers)
          | Gate.Conjunction conState =>
            let nextConState := conState.insert sender pulse
            let nextSignal := if nextConState.toList.all (λ (_, pulse) => pulse == Puls.High) then Puls.Low else Puls.High
            for nextReceiver in nextReceivers do
              q := q.enqueue (receiver, nextReceiver, nextSignal)
            state := state.insert receiver (Gate.Conjunction nextConState, nextReceivers)
          | Gate.Rx _ =>
            if pulse == Puls.Low then
              state := state.insert receiver (Gate.Rx RxState.Done, nextReceivers)
            else
              pure ()
          | Gate.Untyped => pure ()
  return (state, cntLow, cntHigh)

def createGate (name : String) (gate : Gate) (reverseConnections : Std.HashMap String (List String)) : Gate :=
  match gate with
  | Gate.Conjunction _ => reverseConnections.find? name
      |>.map (λ deps => Gate.Conjunction (deps.foldl (λ hm dep =>
        hm.insert dep Puls.Low
      ) Std.HashMap.empty))
      |>.getD gate
  | _ => gate

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ s =>
    match s.splitOn " -> " with
    | [a, b] => (extractType a, b.splitOn ", ")
    | _ => panic! "bad format"
  )

  let reverseConnections := s.foldl (λ state ((_, name), deps) =>
    deps.foldl (λ state dep =>
      match state.find? dep with
      | none => state.insert dep [name]
      | some l => state.insert dep (name :: l)
    ) state
  ) Std.HashMap.empty

  let state := s.foldl (λ state ((gate, name), deps) =>
    state.insert name (createGate name gate reverseConnections, deps)
  ) Std.HashMap.empty

  let mut state := state
  let mut cntLow := 0
  let mut cntHigh := 0
  for _ in [0:1000] do
    let (newState, newCntLow, newCntHigh) ← processSignal state
    state := newState
    cntLow := cntLow + newCntLow
    cntHigh := cntHigh + newCntHigh


  println! "n = {cntLow*cntHigh}"

def findLoop' (state : State) (targetSender : String) (targetReceiver : String) : IO (State × Bool) :=
do
  let mut state := state
  let mut foundSignal : Bool := False
  let mut q := Std.Queue.empty.enqueue ("button", "broadcaster", Puls.Low)
  while ¬q.isEmpty do
    match q.dequeue? with
    | none => panic! "empty queue"
    | some ((sender, receiver, pulse), q') =>
        q := q'
        match state.find? receiver with
        | none => pure () --panic! s!"unknown receiver {receiver}"
        | some (gate, nextReceivers) =>
          match gate with
          | Gate.Broadcaster =>
            for nextReceiver in nextReceivers do
              q := q.enqueue (receiver, nextReceiver, pulse)
          | Gate.FlipFlop flipFlopState =>
            if pulse == Puls.Low then
              let (nextState, nextSignal) :=  match flipFlopState with
                | FlipFlopState.Off => (FlipFlopState.On, Puls.High)
                | FlipFlopState.On => (FlipFlopState.Off, Puls.Low)
              for nextReceiver in nextReceivers do
                q := q.enqueue (receiver, nextReceiver, nextSignal)
              state := state.insert receiver (Gate.FlipFlop nextState, nextReceivers)
          | Gate.Conjunction conState =>
            if receiver == targetReceiver ∧ sender == targetSender ∧ pulse == Puls.High  then
              foundSignal := True
            let nextConState := conState.insert sender pulse
            let nextSignal := if nextConState.toList.all (λ (_, pulse) => pulse == Puls.High) then Puls.Low else Puls.High
            for nextReceiver in nextReceivers do
              q := q.enqueue (receiver, nextReceiver, nextSignal)
            state := state.insert receiver (Gate.Conjunction nextConState, nextReceivers)
          | Gate.Rx _ =>
            if pulse == Puls.Low then
              state := state.insert receiver (Gate.Rx RxState.Done, nextReceivers)
            else
              pure ()
          | Gate.Untyped => pure ()
  return (state, foundSignal)

def findLoop (state : State) (targetSender : String) (targetReceiver : String) : IO Nat :=
do
  let mut state := state
  let mut prev := 0
  for i in [0:1000000000] do
    let (newState, foundSignal) ← findLoop' state targetSender targetReceiver
    state := newState
    if foundSignal then
      println! "found signal at {i} diff: {i - prev}"
      return i
  return 0


def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ s =>
    match s.splitOn " -> " with
    | [a, b] => (extractType a, b.splitOn ", ")
    | _ => panic! "bad format"
  )

  let reverseConnections := s.foldl (λ state ((_, name), deps) =>
    deps.foldl (λ state dep =>
      match state.find? dep with
      | none => state.insert dep [name]
      | some l => state.insert dep (name :: l)
    ) state
  ) Std.HashMap.empty

  let rxRev := reverseConnections.find? "rx"
  let rxRev := match rxRev with
    | none => panic! "rx not found"
    | some l => l
  let rxRev2 := reverseConnections.find? rxRev.head!
  let rxRev2 := match rxRev2 with
    | none => panic! "rx not found"
    | some l => l

  let state := s.foldl (λ state ((gate, name), deps) =>
    state.insert name (createGate name gate reverseConnections, deps)
  ) Std.HashMap.empty

  let state := state.insert "rx" (Gate.Rx RxState.NotDone, [])


  let x ←  rxRev2.mapM (λ rxRev2 =>
    findLoop state rxRev2 rxRev.head!
  )
  let x := x.map (λ x => x + 1)
  let lcm := x.foldl (λ a b => Nat.lcm a b) 1
  IO.println s!"First common divisor is {lcm}"

end P20
end AOC
