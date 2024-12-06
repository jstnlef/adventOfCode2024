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

module Guard =
  let lookAhead guard : int * int =
    Vector2d.add guard.position guard.direction

  let rotate guard : Guard =
    { guard with
        direction = Vector2d.rotate90DegreesCC guard.direction }

  let move guard : Guard =
    { guard with
        position = lookAhead guard }

let isInbounds (x, y) mapState =
  let height = mapState.map.Length
  let width = mapState.map[0].Length
  x >= 0 && x < width && y >= 0 && y < height

let stateIsValid mapState =
  isInbounds mapState.guard.position mapState

let countGuardMovement initialMap =
  let moveGuard mapState _ =
    let x, y = Guard.lookAhead mapState.guard

    let newGuard =
      if mapState.map[y][x] = '#' then
        mapState.guard |> (Guard.rotate >> Guard.move)
      else
        mapState.guard |> Guard.move

    { mapState with
        guard = newGuard
        visited = Set.add newGuard.position mapState.visited }

  Seq.initInfinite id
  |> Seq.scan moveGuard initialMap
  |> Seq.takeWhile stateIsValid
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
