module Common.Itertools

let findFirstFail start goalFunc (array: 'a array) =
  let mutable low = start
  let mutable high = array.Length - 1

  while low < high do
    let middle = (low + high) / 2

    if goalFunc (middle + 1) then
      low <- middle + 1
    else
      high <- middle

  low, array[low]

let countOccurrences elements =
  elements
  |> Seq.fold
    (fun acc element ->
      match Map.tryFind element acc with
      | Some count -> Map.add element (count + 1) acc
      | None -> Map.add element 1 acc)
    Map.empty


let rec cartesianProduct lists =
  match lists with
  | [] -> [ [] ]
  | head :: tail ->
    let tailProduct = cartesianProduct tail

    [ for x in head do
        for y in tailProduct do
          yield x :: y ]
