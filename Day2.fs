module Day2

open System
open Common

let parseReports filename =
  filename |> Input.parseByLine (fun line -> line.Split(" ") |> Array.map int)

module Report =
  let isWithinTolerances sign delta deltaSign =
    let absDelta = abs delta
    (sign = deltaSign || sign = 0) && (absDelta > 0 && absDelta <= 3)

  let compareRawLevels (sign, restIsSafe) (a, b) =
    if not restIsSafe then
      sign, false
    else
      let delta: int = a - b
      let deltaSign = Math.Sign(delta)
      (deltaSign, isWithinTolerances sign delta deltaSign)

  let isSafe report =
    report
    |> Array.pairwise
    |> Seq.scan compareRawLevels (0, true)
    |> Seq.exists (fun (_, isSafe) -> not isSafe)
    |> not

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
