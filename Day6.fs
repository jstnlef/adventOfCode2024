module Day6

open System.IO
open Common

type Guard =
  { position: int * int
    direction: int * int }

type Lab =
  { map: string array
    guard: Guard
    visited: Set<int * int> }

let nextMove guard =
  Vector2d.add guard.position guard.direction

let countGuardMovement initialMap =
  let moveGuard mapState _ =
    let x, y = nextMove mapState.guard

    let newDirection =
      if mapState.map[y][x] = '#' then
        Vector2d.rotate90DegreesCC mapState.guard.direction
      else
        mapState.guard.direction

    let newGuard =
      { position = nextMove mapState.guard
        direction = newDirection }

    { mapState with
        guard = newGuard
        visited = Set.add (x, y) mapState.visited }

  Seq.initInfinite id
  |> Seq.scan moveGuard initialMap
  |> Seq.take 42
  |> Seq.last
  |> _.visited
  |> Seq.length

let parseMap filename : Lab =
  let lines = filename |> File.ReadLines |> Seq.toArray

  let x, y, _ =
    seq {
      for y, line in lines |> Array.indexed do
        for x, c in line |> Seq.indexed do
          yield x, y, c
    }
    |> Seq.find (fun (_, _, c) -> c = '^')

  { map = lines
    guard = { position = x, y; direction = 0, -1 }
    visited = Set [ x, y ] }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day6/test.txt", 41)>]
  [<InlineData("Inputs/Day6/input.txt", -1)>]
  let ``Part 1: Number of spots visited by the guard`` (filename: string, expected: int) =
    let result = filename |> parseMap |> countGuardMovement
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day6/test.txt", -1)>]
  [<InlineData("Inputs/Day6/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) = Assert.True(false)
