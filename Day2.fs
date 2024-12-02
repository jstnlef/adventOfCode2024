module Day2

open Common

let parseReports filename =
  filename |> Input.parseByLine (fun line -> line.Split(" ") |> Array.map int)

module Report =
  let isWithinTolerances acc delta =
    (not (abs delta = 0 || abs delta > 3))
    && (acc <= 0 && delta < 0 || acc >= 0 && delta > 0)

  let compareRawLevels state (a, b) =
    match state with
    | acc, false -> (acc, false)
    | acc, true ->
      let delta = a - b
      (acc + delta, isWithinTolerances acc delta)

  let isSafe report =
    report |> Array.pairwise |> Array.fold compareRawLevels (0, true) |> snd

  let isSafeWithProblemDampener (report: int array) =
    seq { 0 .. report.Length - 1 }
    |> Seq.exists (fun i -> report |> Array.removeAt i |> isSafe)


module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day2/test.txt", 2)>]
  [<InlineData("Inputs/Day2/input.txt", 314)>]
  let ``Part 1: Number of safe reactor reports`` (filename: string, expected: int) =
    let result = filename |> parseReports |> Seq.filter Report.isSafe |> Seq.length
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day2/test.txt", 4)>]
  [<InlineData("Inputs/Day2/input.txt", 373)>]
  let ``Part 2: Number of safe reactor reports with problem dampener`` (filename: string, expected: int) =
    let result =
      filename
      |> parseReports
      |> Seq.filter Report.isSafeWithProblemDampener
      |> Seq.length

    Assert.Equal(expected, result)
