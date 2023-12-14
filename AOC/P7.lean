import Std.Data.HashMap

namespace AOC
namespace P7

inductive HandTypes where
  | fiveOfAKind : HandTypes
  | fourOfAKind : HandTypes
  | fullHouse : HandTypes
  | threeOfAKind : HandTypes
  | twoPairs : HandTypes
  | onePair : HandTypes
  | highCard : HandTypes

instance : Coe HandTypes Nat := ⟨fun c => match c with
  | HandTypes.fiveOfAKind => 1
  | HandTypes.fourOfAKind => 2
  | HandTypes.fullHouse => 3
  | HandTypes.threeOfAKind => 4
  | HandTypes.twoPairs => 5
  | HandTypes.onePair => 6
  | HandTypes.highCard => 7⟩

instance : ToString HandTypes where
  toString t :=
    match t with
    | HandTypes.fiveOfAKind => "Five of a kind"
    | HandTypes.fourOfAKind => "Four of a kind"
    | HandTypes.fullHouse => "Full house"
    | HandTypes.threeOfAKind => "Three of a kind"
    | HandTypes.twoPairs => "Two pairs"
    | HandTypes.onePair => "One pair"
    | HandTypes.highCard => "High card"

instance : Ord HandTypes := ⟨fun c1 c2 => if (c1 : Nat ) > c2 then Ordering.gt else if (c1 : Nat) == c2 then Ordering.eq else Ordering.lt⟩
instance : BEq HandTypes := ⟨fun c1 c2 => (c1 : Nat) == (c2 : Nat)⟩

inductive Card where
  | two : Card
  | three : Card
  | four : Card
  | five : Card
  | six : Card
  | seven : Card
  | eight : Card
  | nine : Card
  | ten : Card
  | jack : Card
  | queen : Card
  | king : Card
  | ace : Card
deriving Repr, Hashable

instance : Coe Card Nat := ⟨fun c => match c with
  | Card.two => 13
  | Card.three => 12
  | Card.four => 11
  | Card.five => 10
  | Card.six => 9
  | Card.seven => 8
  | Card.eight => 7
  | Card.nine => 6
  | Card.ten => 5
  | Card.jack => 4
  | Card.queen => 3
  | Card.king => 2
  | Card.ace => 1⟩

instance : Ord Card := ⟨fun c1 c2 => if (c1 : Nat ) > c2 then Ordering.gt else if (c1 : Nat) == c2 then Ordering.eq else Ordering.lt⟩

instance : BEq Card := ⟨fun c1 c2 => (c1 : Nat) == (c2 : Nat)⟩

instance : Inhabited Card := ⟨Card.two⟩

instance : ToString Card where
  toString c :=
    match c with
    | Card.two => "2"
    | Card.three => "3"
    | Card.four => "4"
    | Card.five => "5"
    | Card.six => "6"
    | Card.seven => "7"
    | Card.eight => "8"
    | Card.nine => "9"
    | Card.ten => "T"
    | Card.jack => "J"
    | Card.queen => "Q"
    | Card.king => "K"
    | Card.ace => "A"

-- mapping from letter to Card
def cardMap (c : String) : Option Card :=
  match c with
  | "2" => Card.two
  | "3" => Card.three
  | "4" => Card.four
  | "5" => Card.five
  | "6" => Card.six
  | "7" => Card.seven
  | "8" => Card.eight
  | "9" => Card.nine
  | "T" => Card.ten
  | "J" => Card.jack
  | "Q" => Card.queen
  | "K" => Card.king
  | "A" => Card.ace
  | _ => none

structure Hand where
  cards : List Card
deriving Repr

instance : ToString Hand where
  toString h := s!"Hand({h.cards})"

def Hand.mkFromString (s: String) : Hand :=
  Hand.mk (s.toList |>.map (λ x => cardMap x.toString) |>.filter (λ x => x.isSome) |>.map (λ x => x.get!))

def Hand.getType (h : Hand) : HandTypes :=
  let m := h.cards.foldl (λ (acc : Std.HashMap Card Nat) (c : Card) => acc.insert c (acc.findD c 0 + 1)) Std.HashMap.empty
  if m.size == 1 then HandTypes.fiveOfAKind
  else if m.size == 2 then
    let v := m.toList |>.map (λ (_, v) => v) |>.maximum? |>.get!
    if v == 4 then HandTypes.fourOfAKind
    else HandTypes.fullHouse
  else if m.size == 3 then
    let v := m.toList |>.map (λ (_, v) => v) |>.maximum? |>.get!
    if v == 3 then HandTypes.threeOfAKind
    else HandTypes.twoPairs
  else if m.size == 4 then HandTypes.onePair
  else
  HandTypes.highCard

def Hand.orderByHighCard (h : Hand) (o : Hand) : Ordering :=
  let x := h.cards.zip o.cards
  -- we compare cards from left to right, and if we find a difference, we return it if not
  -- we return the comparison of the last card
  x.foldl (λ (acc : Ordering) (c : Card × Card) => if acc == Ordering.eq then Ord.compare c.1 c.2 else acc) Ordering.eq

instance : Ord Hand where
  compare h1 h2 :=
    let t1 := h1.getType
    let t2 := h2.getType
    if t1 == t2 then Hand.orderByHighCard h1 h2
    else Ord.compare t1 t2

structure HandBid where
  cards : Hand
  bet : Nat
deriving Repr

instance : Ord HandBid where
  compare b1 b2 := Ord.compare b1.cards b2.cards

instance : Inhabited HandBid where
  default := HandBid.mk (Hand.mk []) 0

instance : ToString HandBid where
  toString b := s!"HandBid({b.cards} {b.bet})"

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast |>.map (λ x => x.splitOn " ") |>.map (λ x => HandBid.mk (Hand.mkFromString x[0]!) x[1]!.toNat!)
  let t := s.toArray |>.qsortOrd

  -- lets sort s
  let mut rank := t.size
  let mut res := 0
  for b in t do
    -- let t := b.cards.getType
    res := res + rank * b.bet
    rank := rank - 1
    -- IO.println s!"{b} {t} - {rank}*{b.bet} = {res}"
  IO.println s!"{res}"

end P7
end AOC
