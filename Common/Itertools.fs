module Common.Itertools

let rec combinationsOf2 l =
  seq {
    for x in l do
      for y in l do
        if x < y then
          yield [| x; y |]
  }
