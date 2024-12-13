module Common.Vector2d

type Vector2d = int * int

let add (x, y) (x2, y2) = x + x2, y + y2
let addInt64 (x: int64, y: int64) (x2, y2) = x + x2, y + y2

let mul scalar (x, y) = (scalar * x, scalar * y)

let rotate90DegreesC (x, y) = -y, x

let determinant (x1, y1) (x2, y2) : int64 = x1 * y2 - x2 * y1

let length (x1: int64, y1) (x2, y2) : int64 = abs (x2 - x1) + abs (y2 - y1)
