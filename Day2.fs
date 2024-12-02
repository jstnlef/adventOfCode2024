module Day2

open Common

let parseReports filename =
  filename |> Input.parseByLine (fun line -> line.Split(" ") |> Array.map int)

module Report =
  let private compareLevels state (a, b) =
    match state with
    | acc, false -> (acc, false)
    | acc, true ->
      let delta = a - b

      if (abs delta = 0 || abs delta > 3) then
        (acc, false)
      elif (acc <= 0 && delta < 0) || (acc >= 0 && delta > 0) then
        (acc + delta, true)
      else
        (acc, false)

  let isSafe report =
    report |> Array.pairwise |> Array.fold compareLevels (0, true) |> snd

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day2/test.txt", 2)>]
  [<InlineData("Inputs/Day2/input.txt", 314)>]
  let ``Part 1: Number of safe reactor reports`` (filename: string, expected: int) =
    let result = filename |> parseReports |> Seq.filter Report.isSafe |> Seq.length
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day2/test.txt", -1)>]
  [<InlineData("Inputs/Day2/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) = Assert.True(false)
