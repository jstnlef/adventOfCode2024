module Day21

open System
open System.Collections.Generic
open System.IO
open Common

type Keypad = char array array

let solveKeypad (keypad: Keypad) =
  let keyMap =
    keypad
    |> Grid.iter
    |> Seq.map (fun loc -> Grid.get keypad loc, loc)
    |> Seq.filter (fun (key, _) -> key <> ' ')
    |> Map

  let moves = Dictionary()

  for l1 in keyMap.Keys do
    for l2 in keyMap.Keys do
      if l1 = l2 then
        moves[(l1, l2)] <- [ "A" ]
      else
        let mutable possibilities = []
        let loc = keyMap[l1]
        let queue = Queue([ loc, "" ])
        let mutable minimum = Int32.MaxValue
        let mutable optimal = false

        while not optimal && queue.Count > 0 do
          let (x, y), moves = queue.Dequeue()

          let neighbors =
            [| x - 1, y, "<"; x + 1, y, ">"; x, y - 1, "^"; x, y + 1, "v" |]
            |> Array.filter (fun (x, y, _) -> Grid.inBounds keypad (x, y) && Grid.get keypad (x, y) <> ' ')

          for nx, ny, nm in neighbors do
            if Grid.get keypad (nx, ny) = l2 then
              if minimum < (moves.Length + 1) then
                optimal <- true
              else
                minimum <- moves.Length + 1
                possibilities <- (moves + nm + "A") :: possibilities
            else
              queue.Enqueue((nx, ny), moves + nm)

        moves[(l1, l2)] <- possibilities

  moves

let directionalKeypad = [| [| ' '; '^'; 'A' |]; [| '<'; 'v'; '>' |] |]

let numericKeypad =
  [| [| '7'; '8'; '9' |]
     [| '4'; '5'; '6' |]
     [| '1'; '2'; '3' |]
     [| ' '; '0'; 'A' |] |]

let allPathsNumeric = solveKeypad numericKeypad
let allPathsDirectional = solveKeypad directionalKeypad

let findKeyPadInstructions (allPaths: Dictionary<char * char, string list>) code =
  Seq.zip ("A" + code) code
  |> Seq.map (fun (a, b) -> allPaths[(a, b)])
  |> Seq.toList
  |> Itertools.cartesianProduct
  |> List.map (String.concat "")

let findNumericKeyPadInstructions = findKeyPadInstructions allPathsNumeric

let computeLength depth moves : int64 =
  let moveLengthInternal memoized depth moves : int64 =
    let transitions = Seq.zip ("A" + moves) moves

    if depth = 1 then
      transitions
      |> Seq.sumBy (fun (a, b) -> allPathsDirectional[(a, b)] |> List.head |> _.Length)
    else
      transitions
      |> Seq.sumBy (fun (a, b) -> allPathsDirectional[(a, b)] |> List.map (memoized (depth - 1)) |> List.min)

  Functools.memoizeRec2 moveLengthInternal depth moves

let findMinInstructions numRobots code =
  let moves = findNumericKeyPadInstructions code
  let minLength = moves |> List.map (computeLength numRobots) |> List.min
  code, minLength

let codeComplexity (code: string, minSequence: int64) = int64 code[..2] * minSequence

let parse filename = filename |> File.ReadAllLines

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day21/test.txt", 126384)>]
  [<InlineData("Inputs/Day21/input.txt", 132532)>]
  let ``Part 1: Sum of the complexities of the 5 codes with 2 robots`` (filename: string, expected: int64) =
    let result =
      filename |> parse |> Array.sumBy ((findMinInstructions 2) >> codeComplexity)

    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day21/test.txt", 154115708116294L)>]
  [<InlineData("Inputs/Day21/input.txt", 165644591859332L)>]
  let ``Part 2: Sum of the complexities of the 5 codes with 25 robots`` (filename: string, expected: int64) =
    let result =
      filename |> parse |> Array.sumBy ((findMinInstructions 25) >> codeComplexity)

    Assert.Equal(expected, result)
