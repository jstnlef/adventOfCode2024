module Day15

open System.Collections.Generic
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

  let moveRobot warehouse move =
    // Grid.print warehouse.grid

    let moveTargets = List()
    let dx, dy = direction move
    let mutable foundWall = false
    let mutable foundSpace = false
    let mutable x, y = warehouse.robot

    while not foundWall && not foundSpace do
      x <- x + dx
      y <- y + dy
      let c = Grid.get warehouse.grid (x, y)

      if c = '.' then
        foundSpace <- true
      elif c = '#' then
        foundWall <- true
      elif c = 'O' then
        moveTargets.Add(x, y, 'O')
      elif c = '[' then
        foundWall <- true
      elif c = ']' then
        foundWall <- true

    if foundSpace then
      let x, y = warehouse.robot
      warehouse.grid[y][x] <- '.'
      let nx, ny = Vector2d.add warehouse.robot (dx, dy)
      warehouse.grid[ny][nx] <- '@'

      for tx, ty, c in moveTargets do
        warehouse.grid[ty + dy][tx + dx] <- c

      { warehouse with robot = nx, ny }
    else
      warehouse

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

let simulateRobot warehouse =
  warehouse.moves |> Array.fold Warehouse.moveRobot warehouse

let parse filename =
  let text = filename |> File.ReadAllText
  let split = text.Split("\n\n")
  let gridText, moves = split[0], split[1].Replace("\n", "")
  let grid = gridText.Split("\n") |> Array.map _.ToCharArray()

  { grid = grid
    moves = moves.ToCharArray()
    robot = Grid.find grid '@' }

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
      |> simulateRobot
      |> Warehouse.boxGPSCoordinates
      |> Seq.sum

    Assert.Equal(expected, result)
