module Common.Itertools

let rec combinationsOf2 l =
  seq {
    for x in l do
      for y in l do
        if x < y then
          yield [| x; y |]
  }

let countOccurrences elements =
  elements
  |> Seq.fold
    (fun acc element ->
      match Map.tryFind element acc with
      | Some count -> Map.add element (count + 1) acc
      | None -> Map.add element 1 acc)
    Map.empty
