module Day1

open adventOfCode2024.Common

type LocationId = int

type Locations =
  { a: LocationId array
    b: LocationId array }

module Locations =
  let sumOfSmallestDistances locations =
    let a = locations.a |> Array.sort
    let b = locations.b |> Array.sort
    Array.zip a b |> Array.sumBy (fun (a, b) -> abs (a - b))

  let parse filename : Locations =
    let a, b =
      filename
      |> Input.parseByLine (fun s ->
        let split = s.Split("   ")
        int (split[0]), int (split[1]))
      |> Seq.toArray
      |> Array.unzip

    { a = a; b = b }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day1/test.txt", 11)>]
  [<InlineData("Inputs/Day1/input.txt", 2000468)>]
  let ``Sum of smallest distances between location ids`` (filename: string, expected: int) =
    let result = filename |> Locations.parse |> Locations.sumOfSmallestDistances
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day1/test.txt", -1)>]
  [<InlineData("Inputs/Day1/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) = Assert.True(false)
