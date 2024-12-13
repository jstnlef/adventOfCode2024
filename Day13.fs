module Day13

open System.IO
open System.Text.RegularExpressions
open Common
open MathNet.Numerics.LinearAlgebra

type ClawMachine =
  { a: int64 * int64
    b: int64 * int64
    prize: int64 * int64 }

let fewestTokensToWinPrizes machines =
  let buttonPressesToPrize machine =
    let ax, ay = machine.a
    let bx, by = machine.b
    let px, py = machine.prize
    let deltas = matrix [ [ double ax; double bx ]; [ double ay; double by ] ]
    let prize = vector [ double px; double py ]
    let solved = deltas.Solve(prize)
    let a, b = int64 (round solved[0]), int64 (round solved[1])

    if ax * a + bx * b = px && ay * a + by * b = py then
      Some(a, b)
    else
      None

  let cost (a, b) = a * 3L + b

  machines
  |> Seq.map buttonPressesToPrize
  |> Seq.filter _.IsSome
  |> Seq.sumBy (fun presses -> cost presses.Value)


let machineRegex =
  Regex(
    "^Button A: X\+(?<AX>\d+), Y\+(?<AY>\d+)\nButton B: X\+(?<BX>\d+), Y\+(?<BY>\d+)\nPrize: X=(?<PX>\d+), Y=(?<PY>\d+)"
  )

let parse filename : ClawMachine array =
  let parseMachine text =
    let m = machineRegex.Match text

    { a = int m.Groups["AX"].Value, int m.Groups["AY"].Value
      b = int m.Groups["BX"].Value, int m.Groups["BY"].Value
      prize = int m.Groups["PX"].Value, int m.Groups["PY"].Value }

  filename |> File.ReadAllText |> _.Split("\n\n") |> Array.map parseMachine

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day13/test.txt", 480)>]
  [<InlineData("Inputs/Day13/input.txt", 34393)>]
  let ``Part 1: Fewest tokens to spend to win all possible prizes`` (filename: string, expected: int64) =
    let result = filename |> parse |> fewestTokensToWinPrizes
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day13/test.txt", 875318608908L)>]
  [<InlineData("Inputs/Day13/input.txt", 83551068361379L)>]
  let ``Part 2: Fewest tokens to spend to win all possible prizes plus 10_000_000_000_000``
    (filename: string, expected: int64)
    =
    let result =
      filename
      |> parse
      |> Array.map (fun m ->
        { m with
            prize = Vector2d.addInt64 m.prize (10000000000000L, 10000000000000L) })
      |> fewestTokensToWinPrizes

    Assert.Equal(expected, result)
