module Day16

open System.Collections.Generic
open System.IO
open Common

type Maze = char array array

module Maze =
  let find c maze =
    maze |> Grid.iter |> Seq.find (fun pos -> Grid.get maze pos = c)

let findPathWithLowestScore maze =
  let startP = Maze.find 'S' maze
  let seen = HashSet([ startP ])
  let queue = PriorityQueue()
  queue.Enqueue((0, startP, (1, 0)), 0)

  let mutable found = 9999999

  while queue.Count > 0 do
    let cost, pos, dir = queue.Dequeue()
    seen.Add(pos) |> ignore

    for ndir in Grid.cardinalVectors do
      let npos = Vector2d.add pos ndir
      let c = Grid.get maze npos

      let ncost = if dir <> ndir then cost + 1001 else cost + 1

      if Grid.get maze npos = 'E' && ncost < found then
        found <- ncost

      if seen.Contains(npos) |> not && c = '.' then
        queue.Enqueue((ncost, npos, ndir), ncost)

  found

let parse filename =
  filename |> File.ReadAllLines |> Array.map _.ToCharArray()

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day16/test.txt", 7036)>]
  [<InlineData("Inputs/Day16/test2.txt", 11048)>]
  [<InlineData("Inputs/Day16/input.txt", 102460)>]
  let ``Part 1: Lowest score a Reindeer could receive`` (filename: string, expected: int) =
    let result = filename |> parse |> findPathWithLowestScore
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day16/test.txt", -1)>]
  [<InlineData("Inputs/Day16/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
