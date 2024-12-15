module Day15

open System.IO
open Common

type Warehouse =
  { grid: char array array
    moves: char array
    robot: int * int }

module Warehouse =
  let gpsCoordinate (x, y) = 100 * y + x

  let boxGPSCoordinates warehouse =
    warehouse.grid
    |> Grid.iter
    |> Seq.map (fun pos -> pos, (Grid.get warehouse.grid pos))
    |> Seq.filter (fun (_, c) -> c = 'O')
    |> Seq.map (fst >> gpsCoordinate)

  let direction c =
    match c with
    | '^' -> 0, -1
    | '<' -> -1, 0
    | '>' -> 1, 0
    | 'v' -> 0, 1
    | _ -> failwith "Not a valid direction"

  let rec moveObject warehouse objPos moveDir =
    // Grid.print warehouse.grid

    let doMove warehouse (x, y) (nx, ny) =
      let obj = Grid.get warehouse.grid (x, y)
      let nextObj = Grid.get warehouse.grid (nx, ny)

      if nextObj = '.' then
        warehouse.grid[ny][nx] <- obj
        warehouse.grid[y][x] <- '.'

        if obj = '@' then
          { warehouse with robot = nx, ny }
        else
          warehouse
      else
        warehouse

    let nextObjPos = Vector2d.add objPos moveDir
    let nextObj = Grid.get warehouse.grid nextObjPos

    if nextObj = '#' then
      warehouse
    elif nextObj = 'O' then
      let warehouse = moveObject warehouse nextObjPos moveDir
      doMove warehouse objPos nextObjPos
    else
      doMove warehouse objPos nextObjPos

let findRobot grid =
  grid |> Grid.iter |> Seq.find (fun pos -> Grid.get grid pos = '@')

let simulateRobot warehouse =
  warehouse.moves
  |> Array.fold
    (fun warehouse move -> Warehouse.moveObject warehouse warehouse.robot (Warehouse.direction move))
    warehouse

let parse filename =
  let text = filename |> File.ReadAllText
  let split = text.Split("\n\n")
  let gridText, moves = split[0], split[1].Replace("\n", "")
  let grid = gridText.Split("\n") |> Array.map _.ToCharArray()

  { grid = grid
    moves = moves.ToCharArray()
    robot = findRobot grid }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day15/smallTest.txt", 2028)>]
  [<InlineData("Inputs/Day15/biggerTest.txt", 10092)>]
  [<InlineData("Inputs/Day15/input.txt", 1414416)>]
  let ``Part 1: Sum of all boxes' GPS coordinates`` (filename: string, expected: int) =
    let result =
      filename |> parse |> simulateRobot |> Warehouse.boxGPSCoordinates |> Seq.sum

    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day15/smallTest.txt", -1)>]
  [<InlineData("Inputs/Day15/biggerTest.txt", 9021)>]
  [<InlineData("Inputs/Day15/input.txt", -1)>]
  let ``Part 2: Sum of all bigger boxes' GPS coordinates`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
