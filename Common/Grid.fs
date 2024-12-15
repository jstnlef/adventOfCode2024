module Common.Grid

open Common.Vector2d

type Grid<'a> = 'a array array

let cardinalVectors: Vector2d array = [| 1, 0; 0, 1; -1, 0; 0, -1 |]

let eightWayVectors: Vector2d array =
  Array.append cardinalVectors [| 1, 1; -1, 1; -1, -1; 1, -1 |]

let inRegion minW maxW minH maxH (x, y) =
  x >= minW && x < maxW && y >= minH && y < maxH

let inBounds width height (x, y) = inRegion 0 width 0 height (x, y)

let get (grid: Grid<'a>) (x, y) = grid[y][x]

let iter (grid: Grid<'a>) =
  seq {
    for y in 0 .. grid.Length - 1 do
      for x in 0 .. grid[y].Length - 1 do
        yield x, y
  }

let neighborsNoGrid vectors width height (x, y) =
  vectors |> Array.map (add (x, y)) |> Array.filter (inBounds width height)

let neighbors vectors (grid: Grid<'a>) position =
  neighborsNoGrid vectors grid[0].Length grid.Length position

let cardinalNeighbors (grid: Grid<'a>) position = neighbors cardinalVectors grid position

let eightWayNeighbors (grid: Grid<'a>) position = neighbors eightWayVectors grid position
