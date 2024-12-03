module Day3

open System.IO
open System.Text.RegularExpressions

let parseMuls filename =
  let mulRegex = Regex("(mul\((?<left>\d+),(?<right>\d+)\))")

  filename
  |> File.ReadAllText
  |> mulRegex.Matches
  |> Seq.map (fun m -> int m.Groups["left"].Value, int m.Groups["right"].Value)

let doMultiply (a, b) = a * b

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day3/test.txt", 161)>]
  [<InlineData("Inputs/Day3/input.txt", 161085926)>]
  let ``Part 1`` (filename: string, expected: int) =
    let result = filename |> parseMuls |> Seq.sumBy doMultiply
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day3/test.txt", -1)>]
  [<InlineData("Inputs/Day3/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) = Assert.True(false)
