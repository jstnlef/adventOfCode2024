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

type Instruction =
  | Mul of int * int
  | Enable
  | Disable

let parseInstructions filename =
  let mulRegex =
    Regex("(?<mul>mul\((?<left>\d+),(?<right>\d+)\))|(?<do>do\(\))|(?<dont>don't\(\))")

  let parseInstruction (m: Match) =
    if m.Value.StartsWith("mul") then
      Mul(int m.Groups["left"].Value, int m.Groups["right"].Value)
    elif m.Value.StartsWith("don't") then
      Disable
    else
      Enable

  filename |> File.ReadAllText |> mulRegex.Matches |> Seq.map parseInstruction

let runProgram instructions =
  let execute (acc, enable) instruction =
    match instruction with
    | Enable -> acc, true
    | Disable -> acc, false
    | Mul(a, b) -> (if enable then acc + (a * b) else acc), enable

  instructions |> Seq.fold execute (0, true) |> fst

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day3/test.txt", 161)>]
  [<InlineData("Inputs/Day3/input.txt", 161085926)>]
  let ``Part 1`` (filename: string, expected: int) =
    let result = filename |> parseMuls |> Seq.sumBy doMultiply
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day3/test.txt", 48)>]
  [<InlineData("Inputs/Day3/input.txt", 82045421)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = filename |> parseInstructions |> runProgram
    Assert.Equal(expected, result)
