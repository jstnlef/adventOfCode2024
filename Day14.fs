module Day14

open System
open System.Text.RegularExpressions
open Common

type Robot =
  { position: int * int
    velocity: int * int }

let moveRobots width height robots =
  let moveBot robot =
    let x, y = Vector2d.add robot.position robot.velocity

    { robot with
        position = Math.modulo x width, Math.modulo y height }

  robots |> Array.map moveBot

let simulate seconds width height robots =
  seq { 0 .. seconds - 1 }
  |> Seq.fold (fun bots _ -> moveRobots width height bots) robots

let safetyFactor width height robots =
  let midW = width / 2
  let midH = height / 2

  let findQuadrant robot =
    if Grid.inRegion 0 midW 0 midH robot.position then
      0
    elif Grid.inRegion (midW + 1) width 0 midH robot.position then
      1
    elif Grid.inRegion (midW + 1) width (midH + 1) height robot.position then
      2
    elif Grid.inRegion 0 midW (midH + 1) height robot.position then
      3
    else
      -1

  robots
  |> Array.groupBy findQuadrant
  |> Array.filter (fun (i, _) -> i >= 0)
  |> Array.map (snd >> Array.length)
  |> Array.reduce (*)

let robotReg = Regex("^p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)$")

let parse filename =
  let parseRobot line =
    let m = robotReg.Match line
    let px, py = int m.Groups[1].Value, int m.Groups[2].Value
    let vx, vy = int m.Groups[3].Value, int m.Groups[4].Value

    { position = (px, py)
      velocity = (vx, vy) }

  filename |> Input.parseByLine parseRobot |> Seq.toArray

let findTree width height (_, robots) =
  let grid = Array.init height (fun _ -> Array.init width (fun _ -> '.'))

  robots
  |> Array.iter (fun robot ->
    let x, y = robot.position
    grid[y][x] <- 'O')

  let lines = grid |> Array.map String
  let s = String.Join("\n", lines)
  s.Contains("OOOOOOO")

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day14/test.txt", 12, 11, 7)>]
  [<InlineData("Inputs/Day14/input.txt", 225521010, 101, 103)>]
  let ``Part 1: Safety factor after 100 seconds`` (filename: string, expected: int, width: int, height: int) =
    let result =
      filename |> parse |> simulate 100 width height |> safetyFactor width height

    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day14/input.txt", 7774)>]
  let ``Part 2: Find the Christmas Tree Easter egg`` (filename: string, expected: int) =
    let width = 101
    let height = 103
    let robots = filename |> parse

    let result =
      Seq.initInfinite ((+) 1)
      |> Seq.scan (fun (_, bots) i -> i, moveRobots width height bots) (0, robots)
      |> Seq.skip 7000
      |> Seq.takeWhile ((findTree width height) >> not)
      |> Seq.last
      |> fst

    Assert.Equal(expected, result + 1)
