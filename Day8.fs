module Day8

open System.Collections.Generic
open System.IO
open Common

type Antennae =
  { antennae: Dictionary<char, (int * int) list>
    width: int
    height: int }

let findAntinode width height ((x1, y1), (x2, y2)) =
  seq {
    let delta = x2 - x1, y2 - y1
    let antinode = Vector2d.add (x2, y2) delta

    if Grid.inBoundsNoGrid width height antinode then
      yield antinode
  }

let findAntinodesWithHarmonics width height ((x1, y1), (x2, y2)) =
  seq {
    let delta = x2 - x1, y2 - y1
    let mutable antinode = x1, y1

    while Grid.inBoundsNoGrid width height antinode do
      yield antinode
      antinode <- Vector2d.add antinode delta
  }

let countAntinodes findAntinodes antennae =
  let findAntinodesForSignal antennaeForSignal =
    Seq.allPairs antennaeForSignal antennaeForSignal
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.collect (findAntinodes antennae.width antennae.height)

  antennae.antennae.Values
  |> Seq.collect findAntinodesForSignal
  |> Seq.distinct
  |> Seq.length

let parse filename =
  let antennae = Dictionary<char, (int * int) list>()
  let lines = filename |> File.ReadLines |> Seq.toArray

  for y, s in lines |> Seq.indexed do
    for x, c in s |> Seq.indexed do
      if c <> '.' then
        if not (antennae.ContainsKey(c)) then
          antennae[c] <- [ (x, y) ]
        else
          antennae[c] <- (x, y) :: antennae[c]

  { antennae = antennae
    width = lines[0].Length
    height = lines.Length }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day8/test.txt", 14)>]
  [<InlineData("Inputs/Day8/input.txt", 254)>]
  let ``Part 1: Number of antinode locations`` (filename: string, expected: int) =
    let result = filename |> parse |> countAntinodes findAntinode
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day8/test.txt", 34)>]
  [<InlineData("Inputs/Day8/input.txt", 951)>]
  let ``Part 2: Number of antinode locations with resonant harmonics`` (filename: string, expected: int) =
    let result = filename |> parse |> countAntinodes findAntinodesWithHarmonics
    Assert.Equal(expected, result)
