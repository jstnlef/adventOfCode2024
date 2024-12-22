module Day21

open System.Collections.Generic
open System.IO
open Common

type Keypad = char array array

type KeypadState =
  { loc: int * int
    instructions: char list }

let dirKey ((lx, ly), (rx, ry)) =
  let x, y = rx - lx, ry - ly

  match x, y with
  | 0, 1 -> 'v'
  | 1, 0 -> '>'
  | -1, 0 -> '<'
  | 0, -1 -> '^'
  | _ -> failwith "Unknown Direction"

let generateInstructions (keypad: Keypad) state nextKey =
  let queue = Queue([ state.loc ])
  let seen = HashSet([ state.loc ])
  let pathMap = Dictionary()
  pathMap[state.loc] <- None

  let rec reconstructPath node acc =
    match pathMap[node] with
    | Some parent -> reconstructPath parent (node :: acc)
    | None -> node :: acc

  let mutable found = None

  while found.IsNone && queue.Count > 0 do
    let loc = queue.Dequeue()

    if Grid.get keypad loc = nextKey then
      found <- Some loc
    else
      for nloc in Grid.cardinalNeighbors keypad loc do
        if (seen.Contains(nloc) |> not) then
          seen.Add(nloc) |> ignore
          queue.Enqueue(nloc)
          pathMap[nloc] <- Some loc

  let nextKeyLoc = found.Value
  let path = reconstructPath nextKeyLoc [] |> List.pairwise |> List.map dirKey

  { state with
      loc = nextKeyLoc
      instructions = state.instructions @ path @ [ 'A' ] }

let directionalKeypad = [| [| ' '; '^'; 'A' |]; [| '<'; 'v'; '>' |] |]

let findDirectionalInstructions instructions =
  let initialState = { loc = 2, 0; instructions = [] }

  instructions
  |> Seq.fold (generateInstructions directionalKeypad) initialState
  |> _.instructions

let numericKeypad =
  [| [| '7'; '8'; '9' |]
     [| '4'; '5'; '6' |]
     [| '1'; '2'; '3' |]
     [| ' '; '0'; 'A' |] |]

let findNumericKeyPadInstructions (code: string) =
  let initialState = { loc = 2, 3; instructions = [] }

  code.ToCharArray()
  |> Array.fold (generateInstructions numericKeypad) initialState
  |> _.instructions

let findInstructions code =
  let myInstructions =
    code
    |> findNumericKeyPadInstructions
    |> findDirectionalInstructions
    |> findDirectionalInstructions

  let s = System.String.Concat(myInstructions)
  printfn $"{code}: {s}"
  code, myInstructions

let codeComplexity (code: string, sequences: char list) = int code[0..2] * sequences.Length

let parse filename = filename |> File.ReadAllLines

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day21/test.txt", 126384)>]
  [<InlineData("Inputs/Day21/input.txt", -1)>]
  let ``Part 1: Sum of the complexities of the 5 codes`` (filename: string, expected: int) =
    let result =
      filename |> parse |> Array.map findInstructions |> Array.sumBy codeComplexity

    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day21/test.txt", -1)>]
  [<InlineData("Inputs/Day21/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
