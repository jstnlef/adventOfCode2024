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

let isUpdateCorrect state update =
  let indexMap = update |> Array.mapi (fun i value -> value, i) |> Map.ofArray

  let verifyRule (a, b) =
    match Map.tryFind a indexMap, Map.tryFind b indexMap with
    | Some(i), Some(j) -> i < j
    | _ -> true

  state.rules |> Array.forall verifyRule

let findCorrectUpdates state : int array array =
  state.updates |> Array.filter (isUpdateCorrect state)

let findIncorrectUpdates state : int array array =
  state.updates |> Array.filter (fun update -> not (isUpdateCorrect state update))

let fixUpdates state (update: int array) : int array =
  let doInsertion i v =
    let mutable j = i - 1

    while j >= 0 && j < update.Length do
      update[j + 1] <- update[j]
      j <- j - 1

    update[j + 1] <- v

  let fixRule (a, b) =
    match (Array.tryFindIndex (fun n -> n = a) update), Array.tryFindIndex (fun n -> n = b) update with
    | Some(i), Some(j) ->
      if i > j then
        doInsertion j a
    | _ -> ()

  Array.iter fixRule state.rules
  update

let findMiddle (update: int array) = update[update.Length / 2]

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
  [<InlineData("Inputs/Day5/input.txt", -1)>]
  let ``Part 2: Sum of middle pages of fixed updates to safety manual`` (filename: string, expected: int) =
    let state = filename |> parse

    let result =
      state
      |> findIncorrectUpdates
      |> Array.map (fixUpdates state)
      |> Array.sumBy findMiddle

    Assert.Equal(expected, result)
