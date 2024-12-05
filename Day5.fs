module Day5

open System.IO

type State =
  { rules: (int * int) array
    updates: int array array }

let isUpdateCorrect state update =
  let verifyRule (a, b) =
    let maybeIndexA = Array.tryFindIndex (fun n -> n = a) update
    let maybeIndexB = Array.tryFindIndex (fun n -> n = b) update

    match maybeIndexA, maybeIndexB with
    | Some(i), Some(j) -> i < j
    | _ -> true

  state.rules |> Array.forall verifyRule

let findCorrectUpdates state : int array array =
  state.updates |> Array.filter (isUpdateCorrect state)

let findIncorrectUpdates state : int array array =
  state.updates |> Array.filter (fun update -> not (isUpdateCorrect state update))

let fixUpdates state (update: int array) : int array =
  let fixRule a b =
    let ruleForA = state.rules |> Array.tryFind (fun (ra, rb) -> ra = a && rb = b)
    let ruleForB = state.rules |> Array.tryFind (fun (rb, ra) -> ra = a && rb = b)

    match ruleForA, ruleForB with
    | Some _, _ -> -1
    | None, Some _ -> 1
    | None, None -> 0

  Array.sortWith fixRule update

let findMiddle (update: int array) = update[update.Length / 2]

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
