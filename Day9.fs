module Day9

open System
open System.IO

type Disk = int16 array

module Disk =
  let swap i j (disk: Disk) =
    let temp = disk[i]
    disk[i] <- disk[j]
    disk[j] <- temp

  let defragment (disk: Disk) =
    let mutable frontI = 0
    let mutable backI = disk.Length - 1

    while frontI <> backI do
      let mutable frontV = disk[frontI]
      let mutable backV = disk[backI]

      if frontV >= 0s then frontI <- frontI + 1
      elif backV < 0s then backI <- backI - 1
      else swap frontI backI disk

    disk

  let checksum (disk: Disk) : int64 =
    disk
    |> Array.indexed
    |> Array.sumBy (fun (i, id) -> if id > 0s then int64 i * int64 id else 0)

let parseDiskMap filename : Disk =
  let diskInput =
    filename
    |> File.ReadAllText
    |> _.Trim()
    |> Seq.map (fun c -> Int32.Parse([| c |]))

  let size = diskInput |> Seq.sum
  let disk = Array.init size (fun _ -> -1s)
  let mutable i = 0
  let mutable fileId = 0s

  for inputIndex, segmentSize in diskInput |> Seq.indexed do
    if inputIndex % 2 = 0 then
      for j in 0 .. (segmentSize - 1) do
        disk[i + j] <- fileId

      fileId <- fileId + 1s

    i <- i + segmentSize

  disk

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day9/test.txt", 1928)>]
  [<InlineData("Inputs/Day9/input.txt", 6291146824486L)>]
  let ``Part 1: Checksum after block defragmentation`` (filename: string, expected: int64) =
    let result = filename |> parseDiskMap |> Disk.defragment |> Disk.checksum
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day9/test.txt", 2858)>]
  [<InlineData("Inputs/Day9/input.txt", -1)>]
  let ``Part 2: Checksum after file deefragmentation`` (filename: string, expected: int64) =
    let result = filename |> parseDiskMap |> Disk.defragment |> Disk.checksum
    Assert.Equal(expected, result)
