module Day16

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day16/test.txt", 7036)>]
  [<InlineData("Inputs/Day16/test2.txt", 11048)>]
  [<InlineData("Inputs/Day16/input.txt", -1)>]
  let ``Part 1: Lowest score a Reindeer could receive`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day16/test.txt", -1)>]
  [<InlineData("Inputs/Day16/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
