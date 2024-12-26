module Day22

open System.IO

let secretNumber n initial =
  let generateNext secret =
    let mutable s = secret
    s <- ((s * 64L) ^^^ s) % 16777216L
    s <- ((s / 32L) ^^^ s) % 16777216L
    s <- ((s * 2048L) ^^^ s) % 16777216L
    s

  seq { 1..n } |> Seq.fold (fun s _ -> generateNext s) initial

let parse filename =
  filename |> File.ReadAllLines |> Array.map int64

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day22/test.txt", 37327623)>]
  [<InlineData("Inputs/Day22/input.txt", 21147129593L)>]
  let ``Part 1`` (filename: string, expected: int64) =
    let result = filename |> parse |> Array.sumBy (secretNumber 2000)
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day22/test.txt", -1)>]
  [<InlineData("Inputs/Day22/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
