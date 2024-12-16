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
    |> Seq.filter (fun (_, c) -> c = 'O' || c = '[')
    |> Seq.map (fst >> gpsCoordinate)

  let direction c =
    match c with
    | '^' -> 0, -1
    | '<' -> -1, 0
    | '>' -> 1, 0
    | 'v' -> 0, 1
    | _ -> failwith "Not a valid direction"

  let rec moveSmallBox warehouse (x, y) moveDir =
    let doMove (x, y) (nx, ny) =
      let nextObj = Grid.get warehouse.grid (nx, ny)

      if nextObj = '.' then
        warehouse.grid[ny][nx] <- 'O'
        warehouse.grid[y][x] <- '.'

    let nextPosition = Vector2d.add (x, y) moveDir
    let nextObj = Grid.get warehouse.grid nextPosition

    if nextObj = 'O' then
      moveSmallBox warehouse nextPosition moveDir

    doMove (x, y) nextPosition

  let rec moveBigBox warehouse (x, y) moveDir = warehouse

  let moveRobot warehouse moveDir =
    // Grid.print warehouse.grid

    let doMove (x, y) (nx, ny) =
      let nextObj = Grid.get warehouse.grid (nx, ny)

      if nextObj = '.' then
        warehouse.grid[ny][nx] <- '@'
        warehouse.grid[y][x] <- '.'
        { warehouse with robot = nx, ny }
      else
        warehouse

    let robotPosition = warehouse.robot
    let nx, ny = Vector2d.add robotPosition moveDir
    let nextObj = Grid.get warehouse.grid (nx, ny)

    if nextObj = 'O' then
      moveSmallBox warehouse (nx, ny) moveDir
    elif nextObj = '[' then
      moveBigBox warehouse (nx, ny) moveDir |> ignore
    elif nextObj = ']' then
      moveBigBox warehouse (nx - 1, ny) moveDir |> ignore

    doMove robotPosition (nx, ny)

  let expand warehouse =
    let expandRow (row: char array) =
      let newRow = Array.init (row.Length * 2) (fun _ -> '.')

      for i, c in row |> Array.indexed do
        let ni = 2 * i

        match c with
        | '#' ->
          newRow[ni] <- '#'
          newRow[ni + 1] <- '#'
        | 'O' ->
          newRow[ni] <- '['
          newRow[ni + 1] <- ']'
        | '@' -> newRow[ni] <- '@'
        | _ -> ()

      newRow

    let x, y = warehouse.robot

    { warehouse with
        grid = warehouse.grid |> Array.map expandRow
        robot = x * 2, y }

let findRobot grid =
  grid |> Grid.iter |> Seq.find (fun pos -> Grid.get grid pos = '@')

let simulateRobot warehouse =
  warehouse.moves
  |> Array.fold (fun warehouse move -> Warehouse.moveRobot warehouse (Warehouse.direction move)) warehouse

let simulateBigWarehouseRobot warehouse =
  warehouse.moves
  |> Array.fold (fun warehouse move -> Warehouse.moveRobot warehouse (Warehouse.direction move)) warehouse

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
  [<InlineData("Inputs/Day15/bigBoxSmallTest.txt", -1)>]
  [<InlineData("Inputs/Day15/biggerTest.txt", 9021)>]
  [<InlineData("Inputs/Day15/input.txt", -1)>]
  let ``Part 2: Sum of all bigger boxes' GPS coordinates`` (filename: string, expected: int) =
    let result =
      filename
      |> parse
      |> Warehouse.expand
      |> simulateBigWarehouseRobot
      |> Warehouse.boxGPSCoordinates
      |> Seq.sum

    Assert.Equal(expected, result)
