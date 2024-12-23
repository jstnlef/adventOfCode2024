module Day23

open System.Collections.Generic
open System.IO

type Network = Dictionary<string, HashSet<string>>

let findInterconnectedComputers (network: Network) =
  let connected = HashSet()

  for n1 in network.Keys do
    for n2 in network[n1] do
      for n3 in network[n2] do
        if n1 <> n3 && network[n3].Contains n1 then
          let interconnected =
            [| n1; n2; n3 |] |> Array.sort |> (fun names -> names[0], names[1], names[2])

          connected.Add(interconnected) |> ignore

  connected
  |> Seq.filter (fun (n1, n2, n3) -> n1.StartsWith("t") || n2.StartsWith("t") || n3.StartsWith("t"))
  |> Seq.length

let createNetwork edges : Network =
  let populate (network: Network) (l, r) =
    if not (network.ContainsKey l) then
      network.Add(l, HashSet([ r ]))

    if not (network.ContainsKey r) then
      network.Add(r, HashSet([ l ]))

    network[r].Add(l) |> ignore
    network[l].Add(r) |> ignore
    network

  edges |> Array.fold populate (Dictionary())

let parse filename =
  filename
  |> File.ReadAllLines
  |> Array.map (fun l ->
    let s = l.Split("-")
    s[0], s[1])
  |> createNetwork

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day23/test.txt", 7)>]
  [<InlineData("Inputs/Day23/input.txt", 1304)>]
  let ``Part 1: Sets of 3 computers which start with name starting with t`` (filename: string, expected: int) =
    let result = filename |> parse |> findInterconnectedComputers
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day23/test.txt", -1)>]
  [<InlineData("Inputs/Day23/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
