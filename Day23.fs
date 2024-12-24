module Day23

open System.Collections.Generic
open System.IO

type Network = Dictionary<string, HashSet<string>>

let findInterconnectedComputersWhichStartWithT (network: Network) =
  let connected = HashSet()

  for n1 in network.Keys do
    for n2 in network[n1] do
      for n3 in network[n2] do
        if n1 <> n3 && network[n3].Contains n1 then
          connected.Add([ n1; n2; n3 ] |> List.sort) |> ignore

  connected
  |> Seq.filter (fun machines -> machines |> List.exists _.StartsWith("t"))
  |> Seq.length

let findPasswordForLANParty (network: Network) =
  let connected = HashSet<string list>()

  let rec findConnected computer (computerSet: HashSet<string>) =
    let computers = computerSet |> Seq.toList |> List.sort

    if not (connected.Contains(computers)) then
      connected.Add(computers) |> ignore

      for neighbor in network[computer] do
        if not (computerSet.Contains(neighbor)) && computerSet.IsSubsetOf network[neighbor] then
          computerSet.Add neighbor |> ignore
          findConnected neighbor computerSet

  for computer in network.Keys do
    findConnected computer (HashSet [ computer ])

  connected |> Seq.maxBy _.Length |> String.concat ","

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
    let result = filename |> parse |> findInterconnectedComputersWhichStartWithT
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day23/test.txt", "co,de,ka,ta")>]
  [<InlineData("Inputs/Day23/test2.txt", "co,de,ka,ta")>]
  [<InlineData("Inputs/Day23/input.txt", "ao,es,fe,if,in,io,ky,qq,rd,rn,rv,vc,vl")>]
  let ``Part 2: Password for LAN party`` (filename: string, expected: string) =
    let result = filename |> parse |> findPasswordForLANParty
    Assert.Equal(expected, result)
