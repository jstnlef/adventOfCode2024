module Day11

open System
open System.IO
open Common

let blink numBlinks stones =
  let countStoneGeneration =
    Functools.memoizeRec2 (fun countStoneGeneration blinks stone ->
      if blinks = 0 then
        1L
      elif stone = 0L then
        countStoneGeneration (blinks - 1) 1
      elif (string stone).Length % 2 = 0 then
        let s = (string stone)
        let midpoint = s.Length / 2
        let l = Int64.Parse(s[.. midpoint - 1])
        let r = Int64.Parse(s[midpoint..])

        (countStoneGeneration (blinks - 1) l) + (countStoneGeneration (blinks - 1) r)
      else
        countStoneGeneration (blinks - 1) (stone * 2024L))

  stones |> Array.sumBy (countStoneGeneration numBlinks)

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
