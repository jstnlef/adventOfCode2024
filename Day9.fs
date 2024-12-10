module Day9

open System
open System.IO

type Disk = int16 array
type File = int16 * int * int
type Free = int * int

module Disk =
  let checksum (disk: Disk) : int64 =
    disk
    |> Array.indexed
    |> Array.sumBy (fun (i, id) -> if id > 0s then int64 i * int64 id else 0)

  let moveBlock sourceI targetI (disk: Disk) =
    disk[targetI] <- disk[sourceI]
    disk[sourceI] <- -1s

  let blockDefragment (disk: Disk) =
    let mutable frontI = 0
    let mutable backI = disk.Length - 1

    while frontI < backI do
      if disk[frontI] >= 0s then frontI <- frontI + 1
      elif disk[backI] < 0s then backI <- backI - 1
      else moveBlock backI frontI disk

    disk

  let moveFile (_, sourceI, size) targetI (disk: Disk) =
    for n in 0 .. (size - 1) do
      moveBlock (sourceI + n) (targetI + n) disk

  let fileDefragment (files: File list, freeSpace: Free array, disk: Disk) =
    let mutable free = freeSpace

    let findAndMarkOpenSpace (_, _, size) : int option =
      let maybeOpen =
        free |> Seq.indexed |> Seq.tryFind (fun (_, (_, freeSize)) -> size <= freeSize)

      match maybeOpen with
      | Some(i, (freeI, freeSize)) ->
        if size < freeSize then
          free[i] <- (freeI + size, freeSize - size)
        elif size = freeSize then
          free <- Array.removeAt i free
        else
          ()

        Some freeI
      | None -> None

    for file in files do
      match findAndMarkOpenSpace file with
      | Some i -> moveFile file i disk
      | None -> ()

    disk

let parseDiskMap filename =
  let diskInput =
    filename
    |> File.ReadAllText
    |> _.Trim()
    |> Seq.map (fun c -> Int32.Parse([| c |]))

  let size = diskInput |> Seq.sum
  let disk = Array.init size (fun _ -> -1s)
  let mutable i = 0
  let mutable fileId = 0s

  let mutable files = []
  let mutable free = []

  for inputIndex, segmentSize in diskInput |> Seq.indexed do
    if inputIndex % 2 = 0 then
      for j in 0 .. (segmentSize - 1) do
        disk[i + j] <- fileId

      files <- (fileId, i, segmentSize) :: files
      fileId <- fileId + 1s
    else if segmentSize > 0 then
      free <- (i, segmentSize) :: free

    i <- i + segmentSize

  files, free |> List.rev |> List.toArray, disk

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day9/test.txt", 1928)>]
  [<InlineData("Inputs/Day9/input.txt", 6291146824486L)>]
  let ``Part 1: Checksum after block defragmentation`` (filename: string, expected: int64) =
    let result =
      filename
      |> parseDiskMap
      |> (fun (_, _, disk) -> Disk.blockDefragment disk)
      |> Disk.checksum

    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day9/test.txt", 2858)>]
  [<InlineData("Inputs/Day9/input.txt", -1)>]
  let ``Part 2: Checksum after file defragmentation`` (filename: string, expected: int64) =
    let result = filename |> parseDiskMap |> Disk.fileDefragment |> Disk.checksum

    // 8468892803578 too high
    Assert.Equal(expected, result)
