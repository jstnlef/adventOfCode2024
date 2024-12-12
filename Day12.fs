module Day12

open System.Collections.Generic
open System.IO
open Common

type Region = ISet<int * int>

module Region =
  let area (region: Region) = region.Count

  let perimeter (region: Region) =
    region
    |> Seq.sumBy (fun (x, y) ->
      Grid.cardinalVectors
      |> Array.filter (fun d -> region.Contains(Vector2d.add d (x, y)))
      |> (fun neighbors -> 4 - neighbors.Length))

  let fencePrice region = area region * perimeter region

let findRegions garden : Region seq =
  let seen = HashSet()

  let findRegion (x, y) =
    let plantType = Grid.get garden (x, y)
    let region = HashSet()
    let q = Queue([ (x, y) ])

    while q.Count > 0 do
      let newPos = q.Dequeue()
      region.Add(newPos) |> ignore

      let neighbors =
        Grid.cardinalNeighbors garden newPos
        |> Array.filter (fun p -> Grid.get garden p = plantType && not (seen.Contains(p)))

      for neighbor in neighbors do
        seen.Add(neighbor) |> ignore
        q.Enqueue(neighbor)

    region

  seq {
    for pos in Grid.iter garden do
      if not (seen.Contains(pos)) then
        seen.Add(pos) |> ignore
        yield findRegion pos
  }

let calculateFencePrice garden =
  garden |> findRegions |> Seq.sumBy Region.fencePrice

let parse filename =
  filename |> File.ReadAllLines |> Array.map _.ToCharArray()

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day12/test.txt", 140)>]
  [<InlineData("Inputs/Day12/test2.txt", 1930)>]
  [<InlineData("Inputs/Day12/input.txt", 1400386)>]
  let ``Part 1: Total price of fencing in all regions`` (filename: string, expected: int) =
    let result = filename |> parse |> calculateFencePrice
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day12/test.txt", -1)>]
  [<InlineData("Inputs/Day12/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
