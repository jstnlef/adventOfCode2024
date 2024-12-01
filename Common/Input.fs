module Common.Input

open System.IO

let parseByLine transform = File.ReadLines >> Seq.map transform
