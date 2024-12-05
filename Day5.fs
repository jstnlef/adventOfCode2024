module Day5

open System.IO

type State =
  { rules: Set<string * string>
    updates: string array array }

let isUpdateCorrect state update =
  let indexMap = update |> Array.mapi (fun i value -> value, i) |> Map.ofArray

  let verifyRule (a, b) =
    match Map.tryFind a indexMap, Map.tryFind b indexMap with
    | Some(i), Some(j) -> i < j
    | _ -> true

  state.rules |> Set.forall verifyRule

let findCorrectUpdates state =
  state.updates |> Array.filter (isUpdateCorrect state)

let findIncorrectUpdates state =
  state.updates |> Array.filter (isUpdateCorrect state >> not)

let fixUpdates state update =
  let fixRule a b =
    if Set.contains (a, b) state.rules then 1
    elif Set.contains (b, a) state.rules then -1
    else 0

  Array.sortWith fixRule update

let findMiddle (update: _ array) = int update[update.Length / 2]

let parse filename =
  let text = filename |> File.ReadAllText
  let split = text.Trim().Split("\n\n")

  let rules =
    split[0].Split("\n")
    |> Array.map (fun s ->
      let split = s.Split("|")
      split[0], split[1])
    |> Set

  let updates = split[1].Split("\n") |> Array.map _.Split(",")
  { rules = rules; updates = updates }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day5/test.txt", 143)>]
  [<InlineData("Inputs/Day5/input.txt", 4924)>]
  let ``Part 1: Sum of middle pages of correct updates to safety manual`` (filename: string, expected: int) =
    let result = filename |> parse |> findCorrectUpdates |> Array.sumBy findMiddle
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day5/test.txt", 123)>]
  [<InlineData("Inputs/Day5/input.txt", 6085)>]
  let ``Part 2: Sum of middle pages of fixed updates to safety manual`` (filename: string, expected: int) =
    let state = filename |> parse

    let result =
      state |> findIncorrectUpdates |> Array.sumBy (fixUpdates state >> findMiddle)

    Assert.Equal(expected, result)
