module Day18

open System.Collections.Generic
open Common

let findMinStepsToExit width (bytes: Set<int * int>) =
  let seen = HashSet([ 0, 0 ])
  let queue = Queue([ (0, (0, 0)) ])

  let mutable found = None

  while found.IsNone && queue.Count > 0 do
    let steps, pos = queue.Dequeue()

    if pos = (width - 1, width - 1) then
      found <- Some(steps)
    else
      for npos in Grid.neighborsNoGrid Grid.cardinalVectors width width pos do
        if (not (bytes.Contains npos)) && (not (seen.Contains(npos))) then
          seen.Add(npos) |> ignore
          queue.Enqueue(steps + 1, npos)

  found

let parse filename =
  filename
  |> Input.parseByLine _.Split(",")
  |> Seq.map (fun vals -> int vals[0], int vals[1])

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day18/test.txt", 7, 12, 22)>]
  [<InlineData("Inputs/Day18/input.txt", 71, 1024, 296)>]
  let ``Part 1: Minimum steps to reach the exit`` (filename: string, width: int, bytesToSim: int, expected: int) =
    let result =
      filename
      |> parse
      |> (fun bytes -> bytes |> Seq.take bytesToSim |> Set)
      |> findMinStepsToExit width
      |> Option.get

    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day18/test.txt", -1)>]
  [<InlineData("Inputs/Day18/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
