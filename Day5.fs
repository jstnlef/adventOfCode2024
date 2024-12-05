module Day5

open System.IO

type State =
  { rules: (int * int) array
    updates: int array array }

let parse filename : State =
  let text = filename |> File.ReadAllText
  let split = text.Split("\n\n")

  let rules =
    split[0].Split("\n")
    |> Array.map (fun s ->
      let split = s.Split("|")
      int split[0], int split[1])

  let updates =
    split[1].Trim().Split("\n")
    |> Array.map (fun s -> s.Split(",") |> Array.map int)

  { rules = rules; updates = updates }

let findCorrectUpdates state : int array array =
  let isUpdateCorrect update =
    let indexMap = update |> Array.mapi (fun idx value -> value, idx) |> Map.ofArray

    let verifyRule (a, b) =
      match Map.tryFind a indexMap, Map.tryFind b indexMap with
      | Some(i), Some(j) -> i < j
      | _ -> true

    state.rules |> Array.forall verifyRule

  state.updates |> Array.filter isUpdateCorrect

let findMiddle (update: int array) = update[update.Length / 2]

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day5/test.txt", 143)>]
  [<InlineData("Inputs/Day5/input.txt", 4924)>]
  let ``Part 1: Sum of middle pages of correct updates to safety manual`` (filename: string, expected: int) =
    let result = filename |> parse |> findCorrectUpdates |> Seq.sumBy findMiddle
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day5/test.txt", -1)>]
  [<InlineData("Inputs/Day5/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) = Assert.True(false)
