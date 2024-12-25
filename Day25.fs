module Day25

open System.IO

let numberOfKeyLockCombinations (keys, locks) =
  let mutable n = 0

  for key in keys do
    for lock in locks do
      if Array.zip key lock |> Array.forall (fun (k, l) -> k + l < 6) then
        n <- n + 1

  n

let parse filename =
  let mutable keys = []
  let mutable locks = []

  let parseKeyOrLock (keyOrLock: string) =
    let s = keyOrLock.Split("\n") |> array2D

    let heights =
      seq { 0 .. s[0, *].Length - 1 }
      |> Seq.map (fun i -> s[*, i] |> Array.filter (fun c -> c = '#') |> (fun c -> Array.length c - 1))
      |> Seq.toArray

    if s[0, 0] = '#' then
      locks <- heights :: locks
    else
      keys <- heights :: keys

  filename
  |> File.ReadAllText
  |> _.Trim().Split("\n\n")
  |> Array.iter parseKeyOrLock

  keys, locks

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day25/test.txt", 3)>]
  [<InlineData("Inputs/Day25/input.txt", 3508)>]
  let ``Part 1: Number of unique key and lock combinations`` (filename: string, expected: int) =
    let result = filename |> parse |> numberOfKeyLockCombinations
    Assert.Equal(expected, result)
