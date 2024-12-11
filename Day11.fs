module Day11

open System
open System.IO

let blink numBlinks stones : int64 array =
  let transform stone =
    if stone = 0L then
      [| 1L |]
    elif (string stone).Length % 2 = 0 then
      let s = (string stone)
      let midpoint = s.Length / 2
      [| Int64.Parse(s[.. midpoint - 1]); Int64.Parse(s[midpoint..]) |]
    else
      [| stone * 2024L |]

  seq { 0 .. numBlinks - 1 }
  |> Seq.fold (fun stones _ -> stones |> Array.collect transform) stones

let parse filename =
  filename |> File.ReadAllText |> _.Split(" ") |> Array.map Int64.Parse

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day11/test.txt", 55312)>]
  [<InlineData("Inputs/Day11/input.txt", 191690)>]
  let ``Part 1: Number of mysterious stones after 25 blinks`` (filename: string, expected: int) =
    let result = filename |> parse |> blink 25 |> Array.length
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day11/test.txt", -1)>]
  [<InlineData("Inputs/Day11/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = filename |> parse |> blink 75 |> Array.length
    Assert.Equal(expected, result)
