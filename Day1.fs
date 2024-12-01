module Day1

open Common.Itertools
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

  let similarityScore locations =
    let occurrencesInB = countOccurrences locations.b

    locations.a
    |> Array.sumBy (fun id -> id * (occurrencesInB.TryFind id |> Option.defaultValue 0))

  let parse filename =
    let a, b =
      filename
      |> Input.parseByLine (fun s ->
        let split = s.Split("   ")
        int split[0], int split[1])
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
  [<InlineData("Inputs/Day1/test.txt", 31)>]
  [<InlineData("Inputs/Day1/input.txt", 18567089)>]
  let ``Calculated similarity score`` (filename: string, expected: int) =
    let result = filename |> Locations.parse |> Locations.similarityScore
    Assert.Equal(expected, result)
