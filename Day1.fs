module Day1

open Common

module LocationIds =
  let sumOfSmallestDistances (a, b) =
    Array.zip (Array.sort a) (Array.sort b)
    |> Array.sumBy (fun (a, b) -> abs (a - b))

  let similarityScore (a, b) =
    let occurrencesInB = Itertools.countOccurrences b
    Array.sumBy (fun id -> id * (occurrencesInB.TryFind id |> Option.defaultValue 0)) a

  let parse filename =
    filename
    |> Input.parseByLine (fun s ->
      let split = s.Split("   ")
      int split[0], int split[1])
    |> Seq.toArray
    |> Array.unzip

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day1/test.txt", 11)>]
  [<InlineData("Inputs/Day1/input.txt", 2000468)>]
  let ``Part 1: Sum of smallest distances between location ids`` (filename: string, expected: int) =
    let result = filename |> LocationIds.parse |> LocationIds.sumOfSmallestDistances
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day1/test.txt", 31)>]
  [<InlineData("Inputs/Day1/input.txt", 18567089)>]
  let ``Part 2: Calculated similarity score`` (filename: string, expected: int) =
    let result = filename |> LocationIds.parse |> LocationIds.similarityScore
    Assert.Equal(expected, result)
