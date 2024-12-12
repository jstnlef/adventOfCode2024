module Common.Functools

open System.Collections.Generic

let uncurry f (a, b) = f a b

let memoize (f: 'a -> 'b) =
  let dict = Dictionary<_, _>()

  fun c ->
    match dict.TryGetValue c with
    | true, value -> value
    | _ ->
      let value = f c
      dict.Add(c, value)
      value

let memoize2 f =
  let f' = memoize (fun (x, y) -> f x y)
  fun x y -> f' (x, y)

let memoize3 f =
  let f' = memoize (fun (x, y, z) -> f x y z)
  fun x y z -> f' (x, y, z)

let memoizeRec f =
  let cache = Dictionary<_, _>()

  let rec memoizedF arg1 =
    match cache.TryGetValue(arg1) with
    | true, value -> value
    | false, _ ->
      let result = f memoizedF arg1
      cache[arg1] <- result
      result

  memoizedF

let memoizeRec2 f =
  let cache = Dictionary<_, _>()

  let rec memoizedF arg1 arg2 =
    let key = (arg1, arg2)

    match cache.TryGetValue(key) with
    | true, value -> value
    | false, _ ->
      let result = f memoizedF arg1 arg2
      cache[key] <- result
      result

  memoizedF

let memoizeRec3 f =
  let cache = Dictionary<_, _>()

  let rec memoizedF arg1 arg2 arg3 =
    let key = (arg1, arg2, arg3)

    match cache.TryGetValue(key) with
    | true, value -> value
    | false, _ ->
      let result = f memoizedF arg1 arg2 arg3
      cache[key] <- result
      result

  memoizedF
