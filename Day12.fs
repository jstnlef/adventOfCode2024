module Day12

open System.Collections.Generic
open System.IO
open Common

type Region = ISet<int * int>

module Region =
  let area (region: Region) = region.Count

  let perimeter (region: Region) =
    let countOutsideEdges pos =
      Grid.cardinalVectors
      |> Array.filter (fun d -> region.Contains(Vector2d.add d pos))
      |> (fun neighbors -> 4 - neighbors.Length)

    region |> Seq.sumBy countOutsideEdges

  let sides (region: Region) =
    // Using half vectors to represent the edges
    let cornersForPos (x, y) =
      [| (x - 0.5, y - 0.5)
         (x + 0.5, y - 0.5)
         (x + 0.5, y + 0.5)
         (x - 0.5, y + 0.5) |]

    let numberOfSidesForCorner surrounding =
      let hasAdjacent =
        surrounding |> Array.map (fun (x, y) -> region.Contains((int x, int y)))

      let number = hasAdjacent |> Array.sumBy (fun ad -> if ad then 1 else 0)

      if number = 1 || number = 3 then
        1
      elif
        number = 2
        && (hasAdjacent = [| true; false; true; false |]
            || hasAdjacent = [| false; true; false; true |])
      then
        2
      else
        0

    region
    |> Seq.collect (fun (x, y) -> cornersForPos (float x, float y))
    |> Seq.distinct
    |> Seq.map cornersForPos
    |> Seq.sumBy numberOfSidesForCorner

  let fencePrice region = area region * perimeter region

  let bulkDiscountFencePrice region = area region * sides region

let findRegions garden : Region seq =
  let seen = HashSet()

  let findRegion pos =
    let plantType = Grid.get garden pos
    let region = HashSet()
    let q = Queue([| pos |])

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

let calculateFencePrice priceFunc garden =
  garden |> findRegions |> Seq.sumBy priceFunc

let parse filename =
  filename |> File.ReadAllLines |> Array.map _.ToCharArray()

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day12/test.txt", 140)>]
  [<InlineData("Inputs/Day12/test2.txt", 1930)>]
  [<InlineData("Inputs/Day12/input.txt", 1400386)>]
  let ``Part 1: Total price of fencing in all regions`` (filename: string, expected: int) =
    let result = filename |> parse |> calculateFencePrice Region.fencePrice
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day12/test.txt", 80)>]
  [<InlineData("Inputs/Day12/test2.txt", 1206)>]
  [<InlineData("Inputs/Day12/input.txt", 851994)>]
  let ``Part 2: Total price of fencing in all regions with bulk discount`` (filename: string, expected: int) =
    let result = filename |> parse |> calculateFencePrice Region.bulkDiscountFencePrice
    Assert.Equal(expected, result)
