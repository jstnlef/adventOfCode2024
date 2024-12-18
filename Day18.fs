module Day18

open System.Collections.Generic
open Common

let findMinStepsToExit width bytesToSim (bytes: (int * int) array) =
  let byteSet = HashSet(bytes |> Seq.take bytesToSim)
  let seen = HashSet([ 0, 0 ])
  let queue = Queue([ (0, (0, 0)) ])

  let mutable found = None

  while found.IsNone && queue.Count > 0 do
    let steps, pos = queue.Dequeue()

    if pos = (width - 1, width - 1) then
      found <- Some(steps)
    else
      for npos in Grid.neighborsNoGrid Grid.cardinalVectors width width pos do
        if (byteSet.Contains npos |> not) && (seen.Contains(npos) |> not) then
          seen.Add(npos) |> ignore
          queue.Enqueue(steps + 1, npos)

  found

let findFirstByteToMakeExitUnreachable width minReachable (bytes: (int * int) array) =
  let mutable low = minReachable
  let mutable high = bytes.Length - 1

  while low < high do
    let middle = (low + high) / 2

    if (findMinStepsToExit width (middle + 1) bytes) |> Option.isSome then
      low <- middle + 1
    else
      high <- middle

  bytes[low]

let parse filename =
  filename
  |> Input.parseByLine _.Split(",")
  |> Seq.map (fun vals -> int vals[0], int vals[1])
  |> Seq.toArray

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day18/test.txt", 7, 12, 22)>]
  [<InlineData("Inputs/Day18/input.txt", 71, 1024, 296)>]
  let ``Part 1: Minimum steps to reach the exit`` (filename: string, width: int, bytesToSim: int, expected: int) =
    let result = filename |> parse |> findMinStepsToExit width bytesToSim |> Option.get

    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day18/test.txt", 7, 12, "6,1")>]
  [<InlineData("Inputs/Day18/input.txt", 71, 1024, "28,44")>]
  let ``Part 2: First byte which will make the exit unreachable``
    (filename: string, width: int, minReachable: int, expected: string)
    =
    let result =
      filename
      |> parse
      |> findFirstByteToMakeExitUnreachable width minReachable
      |> (fun (x, y) -> $"{x},{y}")

    Assert.Equal(expected, result)
