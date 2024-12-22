module Day20

open System.Collections.Generic
open System.IO
open Common

let findCheatsGreaterThanTime timeSaved cheatTime distances (path: (int * int) array) =
  let cheatSavesEnoughTime startLoc endLoc cheatTime =
    let endDistance = Grid.get distances endLoc
    let startDistance = Grid.get distances startLoc
    let delta = endDistance - startDistance - cheatTime
    delta >= timeSaved

  let numberOfCheatsAtLoc maxCheatTime startLoc =
    let seen = HashSet([ startLoc ])
    let locsOnPath = HashSet()
    let queue = Queue([ 0, startLoc ])

    while queue.Count > 0 do
      let steps, loc = queue.Dequeue()
      let nsteps = steps + 1

      if nsteps <= maxCheatTime then
        for nloc in Grid.cardinalNeighbors distances loc do
          if (seen.Contains(nloc) |> not) then
            seen.Add(nloc) |> ignore
            queue.Enqueue(nsteps, nloc)

            if cheatSavesEnoughTime startLoc nloc nsteps then
              locsOnPath.Add(startLoc, nloc) |> ignore

    locsOnPath.Count

  path |> Array.sumBy (numberOfCheatsAtLoc cheatTime)

let distancesAndPath start (racetrack: char array array) =
  let distances =
    Array.init racetrack.Length (fun i -> Array.init racetrack[i].Length (fun _ -> -1))

  let mutable path = []

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

      path <- (x, y) :: path
      x <- nx
      y <- ny
      cost <- cost + 1

  distances, path |> List.rev |> List.toArray

let parse filename =
  let racetrack = filename |> File.ReadAllLines |> Array.map _.ToCharArray()
  racetrack, Grid.find racetrack 'S'

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day20/test.txt", 2, 2, 44)>]
  [<InlineData("Inputs/Day20/input.txt", 100, 2, 1429)>]
  let ``Part 1: Number of cheats to save over 100 picoseconds``
    (filename: string, timeSaved: int, cheatTime: int, expected: int)
    =
    let racetrack, start = parse filename
    let distances, path = distancesAndPath start racetrack

    let result = findCheatsGreaterThanTime timeSaved cheatTime distances path

    Assert.Equal(expected, result)

  [<Theory(Skip = "Runs long on input")>]
  [<InlineData("Inputs/Day20/test.txt", 50, 20, 285)>]
  [<InlineData("Inputs/Day20/input.txt", 100, 20, 988931)>]
  let ``Part 2: Number of cheats to save over 100 picoseconds with 20 picosecond cheats``
    (filename: string, timeSaved: int, cheatTime: int, expected: int)
    =
    let racetrack, start = parse filename
    let distances, path = distancesAndPath start racetrack

    let result = findCheatsGreaterThanTime timeSaved cheatTime distances path
    Assert.Equal(expected, result)
