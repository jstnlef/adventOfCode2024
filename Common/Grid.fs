module Common.Grid

open Common.Vector2d

type Grid<'a> = 'a array array

let cardinalVectors: Vector2d array = [| 1, 0; 0, 1; -1, 0; 0, -1 |]

let eightWayVectors: Vector2d array =
  Array.append cardinalVectors [| 1, 1; -1, 1; -1, -1; 1, -1 |]

let inbounds width height (x, y) =
  x >= 0 && x < width && y >= 0 && y < height

let get (grid: Grid<'a>) (x, y) = grid[y][x]

let private neighbors vectors width height (x, y) =
  vectors
  |> Array.map (fun (dx, dy) -> x + dx, y + dy)
  |> Array.filter (inbounds width height)

let cardinalNeighbors (grid: Grid<'a>) position =
  neighbors cardinalVectors grid[0].Length grid.Length position

let eightWayNeighbors (grid: Grid<'a>) position =
  neighbors eightWayVectors grid[0].Length grid.Length position
