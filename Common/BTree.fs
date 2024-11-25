module Common.BTree

open System

type Node<'a, 'b> =
  { Key: 'a
    Value: 'b
    Left: Tree<'a, 'b>
    Right: Tree<'a, 'b> }

and Tree<'a, 'b> =
  | Node of Node<'a, 'b>
  | Empty

let emptyNode key value =
  { Key = key
    Value = value
    Left = Tree.Empty
    Right = Tree.Empty }

let rec insertNode key value =
  function
  | Tree.Empty -> Tree.Node(emptyNode key value)
  | Node node ->
    if key = node.Key then
      Tree.Node { node with Value = value }
    elif key < node.Key then
      Tree.Node
        { node with
            Left = insertNode key value node.Left }
    else
      Tree.Node
        { node with
            Right = insertNode key value node.Right }

let rec searchTreeBy compareFunc =
  function
  | Tree.Empty -> None
  | Node node ->
    if compareFunc node.Key = 0 then
      Some node.Value
    elif compareFunc node.Key < 0 then
      searchTreeBy compareFunc node.Left
    else
      searchTreeBy compareFunc node.Right

let rec searchTree (value: 'b) =
  searchTreeBy (fun (key: 'a) -> (key :> IComparable).CompareTo(value))

let rec between minValue maxValue tree =
  seq {
    match tree with
    | Tree.Empty -> ()
    | Node node ->
      let keepLeft = node.Key >= minValue
      let keepRight = node.Key <= maxValue

      if keepLeft && keepRight then
        yield (node.Key, node.Value)

      if keepLeft then
        yield! between minValue maxValue node.Left

      if keepRight then
        yield! between minValue maxValue node.Right
  }


let height tree =
  let rec height tree n =
    match tree with
    | Tree.Empty -> n
    | Node node -> max (height node.Left n + 1) (height node.Right n + 1)

  height tree 0

let rec count tree =
  match tree with
  | Tree.Empty -> 0
  | Node node -> 1 + (count node.Left) + (count node.Right)
