module Day22

open System.Collections.Generic
open System.IO

let generateSecretNumbers initialSecret =
  let generateNext secret =
    let mutable s = secret
    s <- ((s <<< 6) ^^^ s) % 16777216L
    s <- ((s >>> 5) ^^^ s) % 16777216L
    s <- ((s <<< 11) ^^^ s) % 16777216L
    s

  [| 1..2000 |] |> Array.scan (fun s _ -> generateNext s) initialSecret

let lastSecretNumber = generateSecretNumbers >> Array.last

let maximumNumberOfBananas initialSecrets =
  let priceTotals = Dictionary()

  let populatePriceTotalsForBuyer initialSecret =
    let seen = HashSet()

    let handlePriceWindow priceWindow =
      let deltas =
        priceWindow |> Array.pairwise |> Array.map (fun (a, b) -> b - a) |> Array.toList

      if seen.Contains(deltas) |> not then
        seen.Add(deltas) |> ignore
        let priceAfterSequence = priceWindow[priceWindow.Length - 1]

        match priceTotals.TryGetValue deltas with
        | false, _ -> priceTotals[deltas] <- priceAfterSequence
        | true, v -> priceTotals[deltas] <- v + priceAfterSequence

    initialSecret
    |> generateSecretNumbers
    |> Array.map (fun i -> i % 10L)
    |> Array.windowed 5
    |> Array.iter handlePriceWindow

  initialSecrets |> Array.iter populatePriceTotalsForBuyer
  priceTotals.Values |> Seq.max

let parse = File.ReadAllLines >> Array.map int64

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day22/test.txt", 37327623)>]
  [<InlineData("Inputs/Day22/input.txt", 21147129593L)>]
  let ``Part 1: Find the 2000th secret number`` (filename: string, expected: int64) =
    let result = filename |> parse |> Array.sumBy lastSecretNumber
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day22/test2.txt", 23)>]
  [<InlineData("Inputs/Day22/input.txt", 2445)>]
  let ``Part 2: Maximum number of bananas`` (filename: string, expected: int64) =
    let result = filename |> parse |> maximumNumberOfBananas
    Assert.Equal(expected, result)
