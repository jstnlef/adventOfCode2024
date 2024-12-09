module Day9

open System.IO

let parseDiskMap filename =
  let disk = filename |> File.ReadAllText
  []

module Disk =
  let defragment disk = disk

  let checksum disk = 0L

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day9/test.txt", 1928)>]
  [<InlineData("Inputs/Day9/input.txt", -1)>]
  let ``Part 1`` (filename: string, expected: int64) =
    let result = filename |> parseDiskMap |> Disk.defragment |> Disk.checksum
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day9/test.txt", -1)>]
  [<InlineData("Inputs/Day9/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
