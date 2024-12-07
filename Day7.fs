module Day7

open System.IO

type Equation = { test: int64; operands: int64 array }

let canBeSolved equation =
  let operators = [| (+); (*) |]

  false

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
  [<InlineData("Inputs/Day7/input.txt", -1)>]
  let ``Part 1: Total bridge calibrations`` (filename: string, expected: int64) =
    let result = filename |> parse |> Array.filter canBeSolved |> Array.sumBy _.test
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day7/test.txt", -1)>]
  [<InlineData("Inputs/Day7/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
