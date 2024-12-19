module Day19

open System.IO
open Common

let towelDesignIsPossible =
  let internalCall recCall (patterns: string array, design: string) =
    if design.Length = 0 then
      true
    else
      patterns
      |> Array.exists (fun pattern ->
        design.StartsWith(pattern)
        && (recCall (patterns, design.Substring pattern.Length)))

  Functools.memoizeRec internalCall

let numberOfPossibleTowelDesigns (patterns, designs) =
  designs
  |> Array.sumBy (fun design -> if towelDesignIsPossible (patterns, design) then 1 else 0)

let countPatternCombinations =
  let internalCall recCall (patterns: string array, design: string) =
    if design.Length = 0 then
      1L
    else
      patterns
      |> Array.sumBy (fun pattern ->
        if design.StartsWith(pattern) then
          recCall (patterns, design.Substring pattern.Length)
        else
          0)

  Functools.memoizeRec internalCall

let numberOfAllPatternCombinations (patterns, designs) =
  designs
  |> Array.sumBy (fun design -> countPatternCombinations (patterns, design))

let parse filename =
  let split = filename |> File.ReadAllText |> _.Trim() |> _.Split("\n\n")

  split[0].Split(", "), split[1].Split("\n")

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day19/test.txt", 6)>]
  [<InlineData("Inputs/Day19/input.txt", 347)>]
  let ``Part 1: Number of possible towel designs`` (filename: string, expected: int) =
    let result = filename |> parse |> numberOfPossibleTowelDesigns
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day19/test.txt", 16)>]
  [<InlineData("Inputs/Day19/input.txt", 919219286602165L)>]
  let ``Part 2: Number of all possible pattern combinations`` (filename: string, expected: int64) =
    let result = filename |> parse |> numberOfAllPatternCombinations
    Assert.Equal(expected, result)
