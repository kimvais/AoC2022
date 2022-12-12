﻿module AoC2022.Day12

open AoC2022.Utils


(*
let isValidMove (grid: Node[][]) node x y =
    let heigthDifference = (node.Height - (getNode grid (x,y) ()).Height)
    heigthDifference <= 1L && heigthDifference >= 0L
*)


let printRow row =
    row
    |> List.map (sprintf "%3d ")
    |> String.concat ""
    |> printfn "%s"

let printGrid grid =
    grid |> List.iter printRow
    printfn ""

let getNeighbourCoords (x,y) = seq [ x - 1, y; x, y - 1; x + 1, y; x, y + 1 ]

let charToL (c: char) =
    match c with
    | 'E' -> 27L
    | 'S' -> 0L
    | c' -> int64 c' - int64 'a' + 1L

let loadGrid fn =
    let parseRow x row = row |> Seq.mapi (fun y c -> c |> charToL) |> List.ofSeq

    let grid =
        readInput fn
        |> Seq.mapi (fun x row -> row |> parseRow x)
        |> List.ofSeq

    grid


let findCoodsByValue grid value = 
    let x = grid |> List.findIndex (fun l -> l |> List.contains value)
    let y = grid.[x] |> List.findIndex (fun i -> i = value)
    x, y

let part1 fn () =
    let grid = loadGrid fn

    let width, height = List.length grid.[0], List.length grid

    let start = findCoodsByValue grid 0L
    let goal = findCoodsByValue grid 27L
    // printGrid grid
    printfn $"Start %A{start}"
    printfn $"Goal %A{goal}"

    let neighbours (x, y) =
        let ret = getNeighbourCoords (x,y)  |> Seq.filter (fun (nx, ny) ->
            nx >= 0
            && ny >= 0
            && nx < height 
            && ny < width 
            && match grid.[nx].[ny] - grid.[x].[y] with
               | hd when hd < 2L -> true
               | _ -> false)
        // printfn $"Neighbours for [%d{x}, %d{y}] = %A{ret}"
        ret
    let gScore _ _ = 1.
    let fScore (x, y) (gx, gy) = sqrt ((float gx - float x) ** 2. + (float gy - float y) ** 2.)

    match
        AStar.search
            start
            goal
            { neighbours = neighbours
              gCost = gScore
              fCost = fScore
              maxIterations = None }
    with
    | Some path ->
        path |> Seq.length |> int64 |> (+) -1L
    | None -> 0L

let part2 fn () = 0L
