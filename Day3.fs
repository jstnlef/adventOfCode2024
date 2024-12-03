module Day3

open System.IO
open System.Text.RegularExpressions

type Instruction =
  | Mul of int * int
  | Enable
  | Disable

let parseInstructions onlyMuls filename =
  let mulOnly = "(?<mul>mul\((?<left>\d+),(?<right>\d+)\))"
  let withEnable = "|(?<do>do\(\))|(?<dont>don't\(\))"

  let regex =
    if onlyMuls then
      Regex(mulOnly)
    else
      Regex(mulOnly + withEnable)

  let parseInstruction (m: Match) =
    if m.Value.StartsWith("mul") then
      Mul(int m.Groups["left"].Value, int m.Groups["right"].Value)
    elif m.Value.StartsWith("don't") then
      Disable
    elif m.Value.StartsWith("do") then
      Enable
    else
      failwith "Unknown match!"

  filename |> File.ReadAllText |> regex.Matches |> Seq.map parseInstruction

let parseMulsOnly filename = parseInstructions true filename
let parseAllInstructions filename = parseInstructions false filename

let runProgram instructions =
  let execute (acc, enable) instruction =
    match instruction with
    | Enable -> acc, 1
    | Disable -> acc, 0
    | Mul(a, b) -> acc + (enable * a * b), enable

  instructions |> Seq.fold execute (0, 1) |> fst

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day3/test.txt", 161)>]
  [<InlineData("Inputs/Day3/input.txt", 161085926)>]
  let ``Part 1: Run the 'mul' only program`` (filename: string, expected: int) =
    let result = filename |> parseMulsOnly |> runProgram
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day3/test.txt", 48)>]
  [<InlineData("Inputs/Day3/input.txt", 82045421)>]
  let ``Part 2: Run the program with conditional enables`` (filename: string, expected: int) =
    let result = filename |> parseAllInstructions |> runProgram
    Assert.Equal(expected, result)
