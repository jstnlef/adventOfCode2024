module Day9

open System
open System.IO

type Disk = int8 array

module Disk =
  let defragment disk = disk

  let checksum (disk: Disk) : int64 =
    disk
    |> Array.indexed
    |> Array.sumBy (fun (i, id) -> if id > 0y then int64 i * int64 id else 0)

let parseDiskMap filename : Disk =
  let diskInput =
    filename
    |> File.ReadAllText
    |> _.Trim()
    |> Seq.map (fun c -> Int32.Parse([| c |]))

  let size = diskInput |> Seq.sum
  let disk = Array.init size (fun _ -> -1y)
  let mutable i = 0
  let mutable fileId = 0y

  for inputIndex, segmentSize in diskInput |> Seq.indexed do
    if inputIndex % 2 = 0 then
      for j in 0 .. (segmentSize - 1) do
        disk[i + j] <- fileId

      fileId <- fileId + 1y

    i <- i + segmentSize

  disk

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
