module Day11

open System
open System.IO
open Common

let countStoneGeneration (recursiveF: int -> int64 -> int64) blinks stone =
  if blinks = 0 then
    1L
  elif stone = 0L then
    recursiveF (blinks - 1) 1
  elif (string stone).Length % 2 = 0 then
    let s = (string stone)
    let midpoint = s.Length / 2
    let l = Int64.Parse(s[.. midpoint - 1])
    let r = Int64.Parse(s[midpoint..])

    (recursiveF (blinks - 1) l) + (recursiveF (blinks - 1) r)
  else
    recursiveF (blinks - 1) (stone * 2024L)

let blink numBlinks stones =
  stones |> Array.sumBy (Functools.memoizeRec2 countStoneGeneration numBlinks)

let parse filename =
  filename |> File.ReadAllText |> _.Split(" ") |> Array.map Int64.Parse

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day11/test.txt", 55312)>]
  [<InlineData("Inputs/Day11/input.txt", 191690)>]
  let ``Part 1: Number of mysterious stones after 25 blinks`` (filename: string, expected: int64) =
    let result = filename |> parse |> blink 25
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day11/test.txt", 65601038650482L)>]
  [<InlineData("Inputs/Day11/input.txt", 228651922369703L)>]
  let ``Part 2: Number of mysterious stones after 75 blinks`` (filename: string, expected: int64) =
    let result = filename |> parse |> blink 75
    Assert.Equal(expected, result)
