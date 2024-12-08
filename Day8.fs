module Day8

open System.Collections.Generic
open System.IO
open Common

type Antennae =
  { antennae: Dictionary<char, Set<int * int>>
    width: int
    height: int }

let isInbounds width height (x, y) =
  x >= 0 && x < width && y >= 0 && y < height

let countAntinodes (antennae: Antennae) : int =
  let findAntinodesForAntennaPair ((x1, y1), (x2, y2)) =
    let v = Vector2d.mul 2 (x2 - x1, y2 - y1)
    Vector2d.add (x1, y1) v

  let findAntinodesForSignal (kvp: KeyValuePair<char, Set<int * int>>) =
    let antennaeForSignal = kvp.Value

    Seq.allPairs antennaeForSignal antennaeForSignal
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.map findAntinodesForAntennaPair
    |> Seq.filter (isInbounds antennae.width antennae.height)

  antennae.antennae |> Seq.collect findAntinodesForSignal |> Set |> Set.count

let parse filename =
  let antennae = Dictionary<char, Set<int * int>>()
  let lines = filename |> File.ReadLines |> Seq.toArray

  for y, s in lines |> Seq.indexed do
    for x, c in s |> Seq.indexed do
      if c <> '.' then
        if not (antennae.ContainsKey(c)) then
          antennae[c] <- Set[(x, y)]
        else
          antennae[c] <- antennae[c].Add((x, y))

  { antennae = antennae
    width = lines[0].Length
    height = lines.Length }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day8/test.txt", 14)>]
  [<InlineData("Inputs/Day8/input.txt", 254)>]
  let ``Part 1: Number of antinode locations`` (filename: string, expected: int) =
    let result = filename |> parse |> countAntinodes
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day8/test.txt", -1)>]
  [<InlineData("Inputs/Day8/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
