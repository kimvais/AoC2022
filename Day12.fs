module AoC2022.Day12

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

let getNeighbourCoords (x, y) = seq [ x - 1, y; x, y - 1; x + 1, y; x, y + 1 ]

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

let getValue (grid: int64 list list) (x,y) = grid[x].[y]
let findCoodsByValue grid value =
    let x = grid |> List.findIndex (fun l -> l |> List.contains value)
    let y = grid.[x] |> List.findIndex (fun i -> i = value)
    x, y

let solve compFunc fn startValue targetValue =
    let grid = loadGrid fn

    let width, height = List.length grid.[0], List.length grid

    let start = findCoodsByValue grid startValue
    // printGrid grid
    printfn $"Start %A{start}"

    let neighbours (x, y) =
        let ret =
            getNeighbourCoords (x, y)
            |> Seq.filter (fun (nx, ny) ->
                nx >= 0
                && ny >= 0
                && nx < height
                && ny < width
                && match grid.[nx].[ny] - grid.[x].[y] with
                   | hd when compFunc hd -> true
                   | _ -> false)
        // printfn $"Neighbours for [%d{x}, %d{y}] = %A{ret}"
        ret

    let gScore _ _ = 1.
    let fScore _ = 1.

    let path =
        match
            AStar.search
                start
                { neighbours = neighbours
                  gCost = gScore
                  fCost = fScore
                  isTarget  = getValue grid >> (=) targetValue
                  maxIterations = None }
        with
        | Some p -> p
        | None -> failwith "No path found"

    path |> Seq.map (fun (x,y) -> grid.[x].[y]) |> List.ofSeq |> List.rev

let part1 fn () =
    let path = solve ((>) 2L) fn 0L 27L
    printfn "%A" path
    path |> Seq.length |> int64 |> (+) -1L

let part2 fn () = 
    let path = solve ((<) -2L) fn 27L 1L
    printfn "%A" path
    path |> Seq.length |> int64 |> (+) -1L
