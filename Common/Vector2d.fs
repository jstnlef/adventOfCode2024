module Common.Vector2d

let add (x, y) (x2, y2) = x + x2, y + y2

let rotate90DegreesCC (x, y) =
  let newX = -y
  let newY = x
  (newX, newY)

let determinant (x1, y1) (x2, y2) : int64 = x1 * y2 - x2 * y1

let length (x1: int64, y1) (x2, y2) : int64 = abs (x2 - x1) + abs (y2 - y1)
