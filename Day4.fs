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

let allDirections x =
  [| for y in -1 .. 1 do
       for x in -1 .. 1 do
         if (x, y) <> (0, 0) then
           yield (x, y) |]

let possibleWordPositions (x, y) : (int * int) array seq =
  let dirs = allDirections 1
  seq { [| (5, 0); (6, 0); (7, 0); (8, 0) |] }

let wordIsXMAS (charMap: Dictionary<char, Set<int * int>>) positions =
  let word = "XMAS"

  positions
  |> Array.indexed
  |> Array.forall (fun (i, pos) -> charMap[word[i]].Contains(pos))


let countXMAS (charMap: Dictionary<char, Set<int * int>>) =
  charMap['X']
  |> Seq.collect possibleWordPositions
  |> Seq.filter (wordIsXMAS charMap)
  |> Seq.length

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day4/test.txt", 18)>]
  [<InlineData("Inputs/Day4/input.txt", -1)>]
  let ``Part 1: Number of times XMAS appears`` (filename: string, expected: int) =
    let result = filename |> parseToCharMap |> countXMAS
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day4/test.txt", -1)>]
  [<InlineData("Inputs/Day4/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) = Assert.True(false)
