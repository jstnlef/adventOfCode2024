module Common.Functools

open System.Collections.Generic

let uncurry f (a, b) = f a b

let memoize f =
  let dict = Dictionary<_, _>()

  fun c ->
    let exist, value = dict.TryGetValue c

    match exist with
    | true -> value
    | _ ->
      let value = f c
      dict.Add(c, value)
      value
