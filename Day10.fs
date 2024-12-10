module Day10

open System
open System.IO

type TopographicMap =
  { map: int array array
    trailheads: (int * int) list
    width: int
    height: int }

let rec countTrailheads map =
  let inbounds (x, y) =
    x >= 0 && x < map.width && y >= 0 && y < map.height

  let neighbors (x, y) =
    seq {
      yield 1, 0
      yield 0, 1
      yield -1, 0
      yield 0, -1
    }
    |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
    |> Seq.filter inbounds

  let rec findNumberOfTrails nines elevation node =
    let x, y = node

    if elevation = 9 && map.map[y][x] = 9 then
      Set.add (x, y) nines
    else
      node
      |> neighbors
      |> Seq.filter (fun (x, y) -> map.map[y][x] = elevation + 1)
      |> Seq.collect (findNumberOfTrails nines (elevation + 1))
      |> Set

  map.trailheads |> List.sumBy ((findNumberOfTrails Set.empty 0) >> Set.count)

let parse filename : TopographicMap =
  let map =
    filename
    |> File.ReadAllLines
    |> Array.map (fun s -> s |> Seq.map (fun c -> Int32.Parse([| c |])) |> Seq.toArray)

  let mutable trailheads = []

  for y, s in map |> Seq.indexed do
    for x, c in s |> Seq.indexed do
      if c = 0 then
        trailheads <- trailheads @ [ x, y ]

  { map = map
    trailheads = trailheads
    width = map[0].Length
    height = map.Length }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day10/test.txt", 36)>]
  [<InlineData("Inputs/Day10/input.txt", 557)>]
  let ``Part 1: Sum of scores of all trailheads`` (filename: string, expected: int) =
    let result = filename |> parse |> countTrailheads
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day10/test.txt", -1)>]
  [<InlineData("Inputs/Day10/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
