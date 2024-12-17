module Day17

open System
open System.IO
open System.Text.RegularExpressions

type Cpu = { a: int; b: int; c: int; ip: int }

type OpCode =
  | Adv = 0
  | Bxl = 1
  | Bst = 2
  | Jnz = 3
  | Bxc = 4
  | Out = 5
  | Bdv = 6
  | Cdv = 7

type Computer =
  { cpu: Cpu
    program: int array
    output: string list
    halted: bool }

let runProgram computer =
  let executeCycle computer _ =
    if computer.cpu.ip >= computer.program.Length then
      { computer with halted = true }
    else
      let cpu = computer.cpu
      let opcode = enum<OpCode> computer.program[cpu.ip]
      let operand = computer.program[cpu.ip + 1]

      let comboOperand operand =
        if operand >= 0 && operand <= 3 then operand
        elif operand = 4 then cpu.a
        elif operand = 5 then cpu.b
        elif operand = 6 then cpu.c
        else failwith "Combo operand 7 is reserved"

      match opcode with
      | OpCode.Adv ->
        { computer with
            Computer.cpu.a = cpu.a >>> (comboOperand operand)
            Computer.cpu.ip = cpu.ip + 2 }
      | OpCode.Bxl ->
        { computer with
            Computer.cpu.b = cpu.b ^^^ operand
            Computer.cpu.ip = cpu.ip + 2 }
      | OpCode.Bst ->
        { computer with
            Computer.cpu.b = comboOperand operand % 8
            Computer.cpu.ip = cpu.ip + 2 }
      | OpCode.Jnz ->
        { computer with
            Computer.cpu.ip = if cpu.a <> 0 then operand else cpu.ip + 2 }
      | OpCode.Bxc ->
        { computer with
            Computer.cpu.b = cpu.b ^^^ cpu.c
            Computer.cpu.ip = cpu.ip + 2 }
      | OpCode.Out ->
        { computer with
            Computer.output = computer.output @ [ string (comboOperand operand % 8) ]
            Computer.cpu.ip = cpu.ip + 2 }
      | OpCode.Bdv ->
        { computer with
            Computer.cpu.b = cpu.a >>> (comboOperand operand)
            Computer.cpu.ip = cpu.ip + 2 }
      | OpCode.Cdv ->
        { computer with
            Computer.cpu.c = cpu.a >>> (comboOperand operand)
            Computer.cpu.ip = cpu.ip + 2 }
      | _ -> failwith "Unknown opcode"

  Seq.initInfinite id
  |> Seq.scan executeCycle computer
  |> Seq.takeWhile (_.halted >> not)
  |> Seq.last

let parse filename =
  let stateReg =
    Regex("Register A: (?<a>\d+)\nRegister B: (?<b>\d+)\nRegister C: (?<c>\d+)\n\nProgram: (?<program>(\d,?)+)")

  let m = filename |> File.ReadAllText |> stateReg.Match

  { cpu =
      { a = int m.Groups["a"].Value
        b = int m.Groups["b"].Value
        c = int m.Groups["c"].Value
        ip = 0 }
    program = m.Groups["program"].Value |> _.Split(",") |> Array.map Int32.Parse
    output = []
    halted = false }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day17/test.txt", "4,6,3,5,6,3,5,2,1,0")>]
  [<InlineData("Inputs/Day17/test2.txt", "0,1,2")>]
  [<InlineData("Inputs/Day17/test3.txt", "4,2,5,6,7,7,7,7,3,1,0")>]
  [<InlineData("Inputs/Day17/input.txt", "1,7,6,5,1,0,5,0,7")>]
  let ``Part 1: Program output string`` (filename: string, expected: string) =
    let result = filename |> parse |> runProgram |> _.output |> String.concat ","
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day17/part2Test.txt", 117440)>]
  [<InlineData("Inputs/Day17/input.txt", -1)>]
  let ``Part 2: Lowest initial value of reg A to have program output itself`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
