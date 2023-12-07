namespace AOC
namespace P4

def a (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast

  let mut total := 0

-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
-- Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19

  for line in s do
    let (_,rest) := match line.splitOn ":" with
      | [a, b] => (a, b)
      | _ => panic! "Unexpected format 1"

    let (winningNumbers, myNumbers) := match rest.splitOn "|" with
      | [a, b] => (a, b)
      | _ => panic! "Unexpected format 2"

    let winningNumbers := winningNumbers.trim.splitOn " " |>.filter (fun x => !x.isEmpty) |>.map String.toInt!
    let myNumbers := myNumbers.trim.splitOn " " |>.filter (fun x => !x.isEmpty) |>.map String.toInt!

    let myWinningNumbers := myNumbers.filter (fun x => winningNumbers.contains x)
    -- the score for the winning numbers is calculated by summing the number of winning numbers, but the first
    -- winning number is worth 1 point, and then we double the number of points for each additional winning number
    -- 1, 1, 2, 4, 8..

    if myWinningNumbers.length == 0 then
      continue

    -- lets start by generating the range 0..myWinningNumbers.size
    let range := List.range (myWinningNumbers.length - 1)

    -- lets add 0 to the beginning of the range
    let range := 0 :: range

    -- now we can map each number in the range to 2^x
    let points := range.map (fun x => 2^x)

    -- now we can sum up the number of points
    let points := points.foldl (fun acc x => acc + x) 0

    --IO.println s!"{winningNumbers} - {myNumbers} => {myWinningNumbers} and a score of {points} points"
    total := total + points


  IO.print s!"{total}\n"

def b (filename : String) : IO Unit :=
do
  let f ← IO.FS.readFile filename
  let s := f.splitOn "\n" |>.dropLast

-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
-- Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19

  let mut scores := Array.mkArray s.length 1

  for (line, idx) in s.zip (List.range s.length) do
    let (_,rest) := match line.splitOn ":" with
      | [a, b] => (a, b)
      | _ => panic! "Unexpected format 1"

    let (winningNumbers, myNumbers) := match rest.splitOn "|" with
      | [a, b] => (a, b)
      | _ => panic! "Unexpected format 2"

    let winningNumbers := winningNumbers.trim.splitOn " " |>.filter (fun x => !x.isEmpty) |>.map String.toInt!
    let myNumbers := myNumbers.trim.splitOn " " |>.filter (fun x => !x.isEmpty) |>.map String.toInt!

    let myWinningNumbers := myNumbers.filter (fun x => winningNumbers.contains x)

    if myWinningNumbers.length == 0 then
      continue

    let multiplier := scores.get! idx

    let range := List.range myWinningNumbers.length
    for i in range do
      --IO.println s!"card {idx} wins a copy of {idx+i+1}"
      scores := scores.set! (idx + i + 1) (scores.get! (idx + i + 1) + multiplier)


  --IO.print s!"{scores}\n"

  let total := scores.foldl (fun acc x => acc + x) 0
  IO.print s!"{total}\n"

end P4
end AOC
