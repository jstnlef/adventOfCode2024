module Day9

open System
open System.Collections.Generic
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

  let fileDefragment (files: File list, freeSpace: Map<int, SortedSet<int>>, disk: Disk) =
    let findOpenSpaceForFile fileI fileSize =
      let mutable min = None

      for size in fileSize..9 do
        match freeSpace[size] |> Seq.tryHead with
        | Some(freeI) when freeI < fileI ->
          match min with
          | None -> min <- Some(freeI, size)
          | Some m when (freeI, size) < m -> min <- Some(freeI, size)
          | _ -> ()
        | _ -> ()

      min

    let moveFileToOpenSpace file =
      let _, fileI, fileSize = file

      match findOpenSpaceForFile fileI fileSize with
      | Some(freeI, freeSize) ->
        freeSpace[freeSize].Remove(freeI) |> ignore

        if fileSize < freeSize then
          freeSpace[freeSize - fileSize].Add(freeI + fileSize) |> ignore

        moveFile file freeI disk
      | _ -> ()

    files |> List.iter moveFileToOpenSpace

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
  let free = Array.init 9 (fun size -> size + 1, SortedSet()) |> Map

  for inputIndex, segmentSize in diskInput |> Seq.indexed do
    if inputIndex % 2 = 0 then
      for j in 0 .. (segmentSize - 1) do
        disk[i + j] <- fileId

      files <- (fileId, i, segmentSize) :: files
      fileId <- fileId + 1s
    else if segmentSize > 0 then
      free[segmentSize].Add(i) |> ignore

    i <- i + segmentSize

  files, free, disk

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
  [<InlineData("Inputs/Day9/input.txt", 6307279963620L)>]
  let ``Part 2: Checksum after file defragmentation`` (filename: string, expected: int64) =
    let result = filename |> parseDiskMap |> Disk.fileDefragment |> Disk.checksum
    Assert.Equal(expected, result)
