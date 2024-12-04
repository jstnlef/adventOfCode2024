module Day4

open System.Collections.Generic
open System.IO

let parseToCharMap filename =
  let d = Dictionary<char, Set<int * int>>()
  let lines = filename |> File.ReadLines

  for y, s in lines |> Seq.indexed do
    for x, c in s |> Seq.indexed do
      if not (d.ContainsKey(c)) then
        d[c] <- Set[(x, y)]
      else
        d[c] <- d[c].Add((x, y))

  d

let possibleXMASPositions (x, y) =
  let allDirections =
    seq {
      for y in -1 .. 1 do
        for x in -1 .. 1 do
          if (x, y) <> (0, 0) then
            yield (x, y)
    }

  [| for dx, dy in allDirections do
       yield Array.init 4 (fun i -> x + dx * i, y + dy * i) |]

let wordIsXMAS (charMap: Dictionary<char, Set<int * int>>) positions =
  let word = "XMAS"

  positions
  |> Array.indexed
  |> Array.forall (fun (i, pos) -> charMap[word[i]].Contains(pos))

let countXMAS (charMap: Dictionary<char, Set<int * int>>) =
  charMap['X']
  |> Seq.collect possibleXMASPositions
  |> Seq.filter (wordIsXMAS charMap)
  |> Seq.length

let isCrossMAS (charMap: Dictionary<char, Set<int * int>>) (x, y) =
  let allPositions =
    [| (x - 1, y - 1); (x - 1, y + 1); (x + 1, y + 1); (x + 1, y - 1) |]

  let checkIfCross p1 p2 =
    (charMap['M'].Contains(p1) && charMap['S'].Contains(p2))
    || (charMap['S'].Contains(p1) && charMap['M'].Contains(p2))

  checkIfCross allPositions[0] allPositions[2]
  && checkIfCross allPositions[1] allPositions[3]

let countCrossMAS (charMap: Dictionary<char, Set<int * int>>) =
  charMap['A'] |> Set.filter (isCrossMAS charMap) |> Set.count

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day4/test.txt", 18)>]
  [<InlineData("Inputs/Day4/input.txt", 2397)>]
  let ``Part 1: Number of times XMAS appears`` (filename: string, expected: int) =
    let result = filename |> parseToCharMap |> countXMAS
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day4/test.txt", 9)>]
  [<InlineData("Inputs/Day4/input.txt", 1824)>]
  let ``Part 2: Number of times X-MAS appears`` (filename: string, expected: int) =
    let result = filename |> parseToCharMap |> countCrossMAS
    Assert.Equal(expected, result)
