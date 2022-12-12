module AoC2022.Day12

open AoC2022.Utils

type Node =
    { X: int
      Y: int
      Height: uint64
      Distance: uint64
      Previous: option<Node> }


let createNode x y ht =
    { X = x
      Y = y
      Height =  ht
      Distance = System.UInt64.MaxValue
      Previous = None }

let getAllNodes (grid: Node [] []) =
    grid
    |> Seq.concat
    |> Seq.map (fun n -> n.X, n.Y)
    |> Set.ofSeq

let getNode (grid: (Node) [] []) (x, y) () = grid.[x].[y]

    
let isValidMove (grid: Node[][]) node x y =
    let heigthDifference = (node.Height - (getNode grid (x,y) ()).Height)
    heigthDifference <= 1UL && heigthDifference >= 0UL
    


let printRow row =
    row
    |> Array.map
        (fun n ->
            match n.Distance with
            | System.UInt64.MaxValue -> " .. "
            | n -> n |> sprintf "%3d ")
    |> String.concat ""
    |> printfn "%s"

let printGrid grid =
    grid |> Array.iter printRow
    printfn ""
let getNeighbourCoords x y =
    seq [ x - 1, y
          x, y - 1
          x + 1, y
          x, y + 1 ]

let rec dijkstra (grid: Node [] []) queue =
    match Set.isEmpty queue with
    | true -> grid
    | false ->
        let curr =
            queue
            |> Seq.minBy (fun (x, y) -> grid.[x].[y].Distance)
            |> getNode grid
            <| ()

        let queue' = Set.remove (curr.X, curr.Y) queue

        let neighbours =
            getNeighbourCoords curr.X curr.Y
            |> Seq.filter
                (fun (nx, ny) ->
                    nx >= 0
                    && ny >= 0
                    && nx < Array.length grid
                    && ny < Array.length (Array.head grid)
                    && (queue' |> Set.contains (nx, ny))
                    && (isValidMove grid curr nx ny)
            )
            |> Seq.map (fun (x', y') -> getNode grid (x', y') ())

        neighbours
        |> Seq.iter
            (fun n ->
                let newDistance = curr.Distance + 1UL

                if newDistance < n.Distance then

                    grid.[n.X].[n.Y] <-
                        { n with
                              Distance = newDistance
                              Previous = Some curr })

        dijkstra grid queue'

let charToUL (c:char) =
    match c with
    | 'E' -> 27UL
    | 'S' -> 0UL
    | c' -> uint64 c' - uint64 'a' + 1UL
let loadGrid fn =
    let parseRow x row =
        row
        |> Seq.mapi (fun y c -> c |> charToUL |> createNode x y)
        |> Array.ofSeq

    let grid =
        readInput fn
        |> Seq.mapi (fun x row -> row |> parseRow x)
        |> Array.ofSeq
    printGrid grid
    grid

let zeroStart (grid: Node [] []) =
    grid.[0].[0] <-
        { grid.[0].[0] with
              Height = 0UL
              Distance = 0UL }



let part1 fn () =
    let grid = loadGrid fn
    zeroStart grid

    dijkstra grid (getAllNodes grid)
     |> printGrid
     
    0L

    

let part2 fn () = 0L
