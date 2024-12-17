module Day17

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day17/test.txt", "4,6,3,5,6,3,5,2,1,0")>]
  [<InlineData("Inputs/Day17/input.txt", -1)>]
  let ``Part 1: Program output string`` (filename: string, expected: string) =
    let result = ""
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day17/test.txt", -1)>]
  [<InlineData("Inputs/Day17/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
