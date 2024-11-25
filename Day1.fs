module Day1

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day1/test.txt", 142)>]
  [<InlineData("Inputs/Day1/input.txt", 0)>]
  let ``The sum of calibration values`` (filename: string, expected: int) =
    Assert.True(false)

  // [<Theory>]
  // [<InlineData("Day1/testInput2.txt", 281)>]
  // [<InlineData("Day1/input.txt", 0)>]
  // let ``The sum of calibration values with word digits`` (filename: string, expected: int) =
  //   let result =
  //     filename
  //     |> CalibrationDocument.parse CalibrationDocumentTransforms.withWordDigits
  //     |> Seq.sum
  //
  //   Assert.Equal(expected, result)
