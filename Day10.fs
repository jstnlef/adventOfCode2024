module Day10

open System
open System.IO
open Common

type TopographicMap =
  { map: int array array
    trailheads: (int * int) list
    width: int
    height: int }

let rec countTrailheads map =
  let rec findReachableNines nines elevation pos =
    if Grid.get map.map pos = 9 then
      Set.add pos nines
    else
      let nextElevation = elevation + 1

      pos
      |> Grid.cardinalNeighbors map.map
      |> Seq.filter (fun newPos -> (Grid.get map.map newPos) = nextElevation)
      |> Seq.collect (findReachableNines nines nextElevation)
      |> Set

  map.trailheads |> List.sumBy ((findReachableNines Set.empty 0) >> Set.count)

let rec countRatingsForTrailheads map =
  let rec findNumberOfTrails elevation pos =
    if Grid.get map.map pos = 9 then
      1
    else
      let nextElevation = elevation + 1

      pos
      |> Grid.cardinalNeighbors map.map
      |> Seq.filter (fun newPos -> (Grid.get map.map newPos) = nextElevation)
      |> Seq.sumBy (findNumberOfTrails nextElevation)

  map.trailheads |> List.sumBy (findNumberOfTrails 0)

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
  [<InlineData("Inputs/Day10/test.txt", 81)>]
  [<InlineData("Inputs/Day10/input.txt", 1062)>]
  let ``Part 2: Sum of the ratings of all trailheads`` (filename: string, expected: int) =
    let result = filename |> parse |> countRatingsForTrailheads
    Assert.Equal(expected, result)
