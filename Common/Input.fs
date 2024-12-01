module adventOfCode2024.Common.Input

open System.IO

let parseByLine transform filename =
  filename |> File.ReadLines |> Seq.map transform
