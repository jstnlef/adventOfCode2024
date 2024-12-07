module Day7

open System.IO

type Equation = { test: int64; operands: int64 array }

let canBeSolved operators equation =
  let rec doSolve acc (remaining: int64 array) =
    if Array.isEmpty remaining then
      equation.test = acc
    elif acc > equation.test then
      false
    else
      operators
      |> Array.exists (fun op -> doSolve (op acc remaining[0]) remaining[1..])

  doSolve 0 equation.operands


let parse filename =
  let parseLine (line: string) =
    let split = line.Split(":")

    { test = int64 split[0]
      operands = split[1].Trim().Split(" ") |> Array.map int64 }

  filename |> File.ReadLines |> Seq.map parseLine |> Seq.toArray

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day7/test.txt", 3749)>]
  [<InlineData("Inputs/Day7/input.txt", 1545311493300L)>]
  let ``Part 1: Total bridge calibrations`` (filename: string, expected: int64) =
    let result =
      filename
      |> parse
      |> Array.filter (canBeSolved [| (+); (*) |])
      |> Array.sumBy _.test

    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day7/test.txt", 11387)>]
  [<InlineData("Inputs/Day7/input.txt", 169122112716571L)>]
  let ``Part 2`` (filename: string, expected: int64) =
    let result =
      filename
      |> parse
      |> Array.filter (canBeSolved [| (+); (*); (fun a b -> int64 $"{a}{b}") |])
      |> Array.sumBy _.test

    Assert.Equal(expected, result)
