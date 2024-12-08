module Day6

open System.IO
open Common

type Guard =
  { position: int * int
    direction: int * int }

type Lab =
  { map: string array
    guard: Guard
    visited: Set<Guard>
    hasLoop: bool
    terminal: bool }

module Guard =
  let lookAhead guard : int * int =
    Vector2d.add guard.position guard.direction

  let rotate guard : Guard =
    { guard with
        direction = Vector2d.rotate90DegreesC guard.direction }

  let move guard : Guard =
    { guard with
        position = lookAhead guard }

let simulateGuardPatrol (newObstacle: Option<int * int>) initial =
  let isInbounds (x, y) =
    let height = initial.map.Length
    let width = initial.map[0].Length
    x >= 0 && x < width && y >= 0 && y < height

  let willHitNewObstacle (x, y) =
    match newObstacle with
    | Some(obX, obY) -> (x, y) = (obX, obY)
    | None -> false

  let moveGuard lab =
    let x, y = Guard.lookAhead lab.guard

    if lab.hasLoop || not (isInbounds (x, y)) then
      { lab with terminal = true }
    else
      let newGuard =
        if lab.map[y][x] = '#' || willHitNewObstacle (x, y) then
          lab.guard |> Guard.rotate
        else
          lab.guard |> Guard.move

      let hasLoop = Set.contains newGuard lab.visited

      { lab with
          guard = newGuard
          visited = Set.add newGuard lab.visited
          hasLoop = hasLoop }

  Seq.initInfinite id
  |> Seq.scan (fun state _ -> moveGuard state) initial
  |> Seq.takeWhile (_.terminal >> not)
  |> Seq.last

let parseMap filename : Lab =
  let lines = filename |> File.ReadLines |> Seq.toArray

  let x, y, _ =
    seq {
      for y, line in lines |> Array.indexed do
        for x, c in line |> Seq.indexed do
          yield x, y, c
    }
    |> Seq.find (fun (_, _, c) -> c = '^')

  let guard = { position = x, y; direction = 0, -1 }

  { map = lines
    guard = guard
    visited = Set [ guard ]
    hasLoop = false
    terminal = false }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day6/test.txt", 41)>]
  [<InlineData("Inputs/Day6/test2.txt", 91)>]
  [<InlineData("Inputs/Day6/input.txt", 4602)>]
  let ``Part 1: Number of spots visited by the guard`` (filename: string, expected: int) =
    let result =
      filename
      |> parseMap
      |> simulateGuardPatrol None
      |> _.visited
      |> Set.map _.position
      |> Set.count

    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day6/test.txt", 6)>]
  [<InlineData("Inputs/Day6/test2.txt", 19)>]
  [<InlineData("Inputs/Day6/input.txt", 1703)>]
  let ``Part 2: Number of possible obstructions for guard loops`` (filename: string, expected: int) =
    let initial = filename |> parseMap

    let result =
      initial
      |> simulateGuardPatrol None
      |> _.visited
      |> Set.map _.position
      |> Seq.filter (fun p -> p <> initial.guard.position && simulateGuardPatrol (Some p) initial |> _.hasLoop)
      |> Seq.length

    Assert.Equal(expected, result)
