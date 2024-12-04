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

let possibleWordPositions (x, y) : (int * int) array seq =
  seq { [| (5, 0); (6, 0); (7, 0); (8, 0) |] }

let countXMAS (charMap: Dictionary<char, Set<int * int>>) =
  let word = "XMAS"
  let possibleStarts = charMap['X']

  possibleStarts |> Seq.map possibleWordPositions

  0

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
