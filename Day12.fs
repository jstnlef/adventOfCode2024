module Day12

let calculateFencePrice _ = 0

let parse filename = 0

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day12/test.txt", 140)>]
  [<InlineData("Inputs/Day12/test2.txt", 1930)>]
  [<InlineData("Inputs/Day12/input.txt", -1)>]
  let ``Part 1: Total price of fencing in all regions`` (filename: string, expected: int) =
    let result = filename |> parse |> calculateFencePrice
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day12/test.txt", -1)>]
  [<InlineData("Inputs/Day12/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
