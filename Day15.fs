module Day15

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day15/smallTest.txt", 2028)>]
  [<InlineData("Inputs/Day15/biggerTest.txt", 10092)>]
  [<InlineData("Inputs/Day15/input.txt", -1)>]
  let ``Part 1`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day15/smallTest.txt", -1)>]
  [<InlineData("Inputs/Day15/biggerTest.txt", -1)>]
  [<InlineData("Inputs/Day15/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
