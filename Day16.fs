module Day16

open System
open System.Collections.Generic
open System.IO
open Common

type Maze = char array array

let findPathWithLowestScore maze =
  let startP = Grid.find maze 'S'
  let seen = HashSet([ startP ])
  let queue = SortedSet([ (0, startP, (1, 0)) ])

  let mutable minCost = Int32.MaxValue

  while (queue.Count > 0) && (minCost = Int32.MaxValue) do
    let v = queue.Min
    queue.Remove(v) |> ignore
    let cost, pos, dir = v
    seen.Add(pos) |> ignore

    if Grid.get maze pos = 'E' && cost < minCost then
      minCost <- cost

    for ndir in Grid.cardinalVectors do
      let npos = Vector2d.add pos ndir
      let c = Grid.get maze npos

      if seen.Contains(npos) |> not && (c = '.' || c = 'E') then
        let ncost = if dir <> ndir then cost + 1001 else cost + 1
        queue.Add((ncost, npos, ndir)) |> ignore

  minCost

let findNumberOfBestSeats maze =
  let startP = Grid.find maze 'S'
  let seen = HashSet([ (startP, (1, 0)) ])
  let queue = SortedSet([ (0, startP, (1, 0)) ])
  let backtrack = Dictionary()

  while (queue.Count > 0) do
    let v = queue.Min
    queue.Remove(v) |> ignore
    let cost, pos, dir = v
    seen.Add((pos, dir)) |> ignore

    if Grid.get maze pos = 'E' then
      // minCost <- cost
      ()

    for ndir in Grid.cardinalVectors do
      let npos = Vector2d.add pos ndir
      let c = Grid.get maze npos

      if seen.Contains((npos, ndir)) |> not && (c = '.' || c = 'E') then
        let ncost = if dir <> ndir then cost + 1001 else cost + 1

        let hasValue, positions = backtrack.TryGetValue(npos)

        if not hasValue then
          backtrack[npos] <- [ cost, pos ]
        elif ncost < (positions.Head |> fst) then
          backtrack[npos] <- [ cost, pos ]
        elif ncost = (positions.Head |> fst) then
          backtrack[npos] <- (cost, pos) :: positions

        queue.Add((ncost, npos, ndir)) |> ignore


  0

let parse filename =
  filename |> File.ReadAllLines |> Array.map _.ToCharArray()

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day16/test.txt", 7036)>]
  [<InlineData("Inputs/Day16/test2.txt", 11048)>]
  [<InlineData("Inputs/Day16/input.txt", 102460)>]
  let ``Part 1: Lowest score a Reindeer could receive`` (filename: string, expected: int) =
    let result = filename |> parse |> findPathWithLowestScore
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day16/test.txt", 45)>]
  [<InlineData("Inputs/Day16/test2.txt", 64)>]
  [<InlineData("Inputs/Day16/input.txt", -1)>]
  let ``Part 2: Number of best seats`` (filename: string, expected: int) =
    let result = filename |> parse |> findNumberOfBestSeats
    Assert.Equal(expected, result)
