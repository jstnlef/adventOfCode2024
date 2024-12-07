module Day7

open System.IO

type Equation = { test: int64; operands: int64 list }

let canBeSolved operators equation =
  let rec isSolved acc remaining =
    if acc > equation.test then
      false
    else
      match remaining with
      | [] -> acc = equation.test
      | head :: tail -> operators |> Array.exists (fun op -> isSolved (op acc head) tail)

  isSolved 0 equation.operands

let parse filename =
  let parseLine (line: string) =
    let split = line.Split(": ")

    { test = int64 split[0]
      operands = split[1].Split(" ") |> Array.map int64 |> Array.toList }

  filename |> File.ReadLines |> Seq.map parseLine

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day7/test.txt", 3749)>]
  [<InlineData("Inputs/Day7/input.txt", 1545311493300L)>]
  let ``Part 1: Total bridge calibrations`` (filename: string, expected: int64) =
    let result =
      filename |> parse |> Seq.filter (canBeSolved [| (+); (*) |]) |> Seq.sumBy _.test

    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day7/test.txt", 11387)>]
  [<InlineData("Inputs/Day7/input.txt", 169122112716571L)>]
  let ``Part 2: Total bridge calibrations with concat`` (filename: string, expected: int64) =
    let result =
      filename
      |> parse
      |> Seq.filter (canBeSolved [| (+); (*); (fun a b -> int64 $"{a}{b}") |])
      |> Seq.sumBy _.test

    Assert.Equal(expected, result)
