open System
open System.IO

let getArguments (args: string array) =
  if args.Length > 1 then args[1..] else [||]

let args = fsi.CommandLineArgs |> getArguments

// Check if the correct number of arguments are provided
if args.Length <> 1 then
  printfn "Usage: fsi CreateDay.fsx <DayNum>"
else
  let dayName = $"Day{args[0]}"
  let dayPath = $"{dayName}.fs"

  if File.Exists(dayPath) then
    printfn "Day already exists. Exiting"
    Environment.Exit(1)

  // Create input directory
  let inputDirectory = $"Inputs/{dayName}"
  Directory.CreateDirectory(inputDirectory) |> ignore
  // Create stub files
  File.Create($"{inputDirectory}/test.txt").Dispose()
  File.Create($"{inputDirectory}/input.txt").Dispose()

  let content =
    $"""module {dayName}

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("{inputDirectory}/test.txt", -1)>]
  [<InlineData("{inputDirectory}/input.txt", -1)>]
  let ``Part 1`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("{inputDirectory}/test.txt", -1)>]
  [<InlineData("{inputDirectory}/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)"""

  use writer = new StreamWriter(dayPath)
  writer.WriteLine(content)

  printfn $"'{dayName}' created successfully"
