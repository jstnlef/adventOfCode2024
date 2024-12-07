module Common.Itertools

let rec combinationsOf2 l =
  seq {
    for x in l do
      for y in l do
        if x < y then
          yield [| x; y |]
  }

let rec combinations k lst =
  match k, lst with
  | 0, _ -> seq [ [] ]
  | _, [] -> Seq.empty
  | k, x :: xs ->
    let withX = combinations (k - 1) xs |> Seq.map (fun tail -> x :: tail)
    let withoutX = combinations k xs
    Seq.append withX withoutX

let rec combinationsWithReplacement k lst =
  match k, lst with
  | 0, _ -> seq [ [] ]
  | _, [] -> Seq.empty
  | k, x :: xs ->
    seq {
      for combo in combinationsWithReplacement (k - 1) (x :: xs) do
        yield x :: combo

      for combo in combinationsWithReplacement k xs do
        yield combo
    }

let countOccurrences elements =
  elements
  |> Seq.fold
    (fun acc element ->
      match Map.tryFind element acc with
      | Some count -> Map.add element (count + 1) acc
      | None -> Map.add element 1 acc)
    Map.empty
