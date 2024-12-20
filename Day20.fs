module Day20

open System.IO
open Common

let findCheatsGreaterThanTime savedTime start distances = 0

let distanceGrid start (racetrack: char array array) =
  let distances =
    Array.init racetrack.Length (fun i -> Array.init racetrack[i].Length (fun _ -> -1))

  let mutable x, y = start
  let mutable cost = 0
  let mutable atEnd = false

  while not atEnd do
    distances[y][x] <- cost

    if Grid.get racetrack (x, y) = 'E' then
      atEnd <- true
    else
      let nx, ny =
        Grid.cardinalNeighbors racetrack (x, y)
        |> Array.find (fun loc ->
          let c = Grid.get racetrack loc
          (c = '.' || c = 'E') && Grid.get distances loc < 0)

      x <- nx
      y <- ny
      cost <- cost + 1

  distances

let parse filename =
  let racetrack = filename |> File.ReadAllLines |> Array.map _.ToCharArray()
  racetrack, Grid.find racetrack 'S'

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day20/test.txt", 1, 44)>]
  [<InlineData("Inputs/Day20/input.txt", 100, -1)>]
  let ``Part 1: Number of cheats to save over 100 picoseconds`` (filename: string, savedTime: int, expected: int) =
    let racetrack, start = parse filename

    let result =
      findCheatsGreaterThanTime savedTime start (distanceGrid start racetrack)

    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day20/test.txt", -1)>]
  [<InlineData("Inputs/Day20/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
