module Common.Functools

open System.Collections.Concurrent

let uncurry f (a, b) = f a b

let memoize (f: 'a -> 'b) =
  let cache = ConcurrentDictionary<'a, Lazy<'b>>()
  fun x -> cache.GetOrAdd(x, lazy (f x)).Value

let memoize2 (f: 'a -> 'b -> 'c) =
  let f' = memoize (fun (x, y) -> f x y)
  fun x y -> f' (x, y)

let memoize3 (f: 'a -> 'b -> 'c -> 'd) =
  let f' = memoize (fun (x, y, z) -> f x y z)
  fun x y z -> f' (x, y, z)

let memoizeRec f =
  let cache = ConcurrentDictionary<_, Lazy<_>>()

  let rec recF x =
    cache.GetOrAdd(x, lazy (f recF x)).Value

  recF

let memoizeRec2 f =
  let cache = ConcurrentDictionary<_, Lazy<_>>()

  let rec recF x y =
    cache.GetOrAdd((x, y), lazy (f recF x y)).Value

  recF

let memoizeRec3 f =
  let cache = ConcurrentDictionary<_, Lazy<_>>()

  let rec recF x y z =
    cache.GetOrAdd((x, y, z), lazy (f recF x y z)).Value

  recF
