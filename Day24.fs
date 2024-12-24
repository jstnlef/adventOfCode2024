module Day24

open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type Gate =
  | AND
  | OR
  | XOR

module Gate =
  let parse s =
    match s with
    | "AND" -> AND
    | "OR" -> OR
    | "XOR" -> XOR
    | _ -> failwith "Unknown gate"

  let op gate =
    match gate with
    | AND -> (&&&)
    | OR -> (|||)
    | XOR -> (^^^)

type Device =
  { inputs: Dictionary<string, int>
    gates: Dictionary<string, Gate * string * string> }

module Device =
  let output device : int64 =
    let wireOutputs = Dictionary(device.inputs)

    let rec computeWire wire =
      if wireOutputs.ContainsKey(wire) then
        wireOutputs[wire]
      else
        let gate, wireA, wireB = device.gates[wire]
        let result = (Gate.op gate) (computeWire wireA) (computeWire wireB)
        wireOutputs[wire] <- result
        result

    seq { 0..64 }
    |> Seq.map (fun i -> $"z%02d{i}")
    |> Seq.takeWhile device.gates.ContainsKey
    |> Seq.indexed
    |> Seq.sumBy (fun (i, wire) -> computeWire wire <<< i)

let parse filename : Device =
  let split = filename |> File.ReadAllText |> _.Trim().Split("\n\n")
  let inputLineReg = Regex("(\w+): (\d)")
  let inputs = Dictionary()

  split[0]
  |> _.Split("\n")
  |> Array.iter (fun line ->
    let m = inputLineReg.Match line
    inputs.Add(m.Groups[1].Value, int m.Groups[2].Value))

  let gatesLineReg = Regex("(\w+) (\w+) (\w+) -> (\w+)")
  let gates = Dictionary()

  split[1]
  |> _.Split("\n")
  |> Array.iter (fun line ->
    let m = gatesLineReg.Match line
    gates.Add(m.Groups[4].Value, (Gate.parse m.Groups[2].Value, m.Groups[1].Value, m.Groups[3].Value)))

  { inputs = inputs; gates = gates }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day24/test.txt", 2024)>]
  [<InlineData("Inputs/Day24/input.txt", 58639252480880L)>]
  let ``Part 1: Output decimal number`` (filename: string, expected: int64) =
    let result = filename |> parse |> Device.output
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day24/test.txt", -1)>]
  [<InlineData("Inputs/Day24/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int64) =
    let result = 0
    Assert.Equal(expected, result)
