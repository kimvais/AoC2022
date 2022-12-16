module AoC2022.Day16

open System.Text.RegularExpressions
open AoC2022.Utils


type Valve = { Name: string; FlowRate: int64; Neighbours: string list }
let valveRe =
    Regex @"^Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z, ]+)"

let reToValve (s) =
    let m = valveRe.Match(s)
    let neighbours = m.Groups.[3].Value |> splitS ", " |> List.ofArray

    let valve =
        { Name = m.Groups.[1].Value
          FlowRate = m.Groups.[2].Value |> int64
          Neighbours = neighbours }

    printfn $"%A{valve}"
    valve
    

let dfs valveMap (startNode: Valve) =
    let rec dfsHelper (currentNode: Valve) (visited: string list) =
        let neighbors = currentNode.Neighbours
        let updatedVisited = currentNode.Name :: visited
        let unvisitedNeighbors =
            neighbors |> List.filter (fun neighbor -> not (List.contains (Map.find neighbor valveMap).Name visited))
        let visitedNeighbors =
            unvisitedNeighbors |> List.map (fun neighbor -> dfsHelper (Map.find neighbor valveMap) updatedVisited)
            |> List.concat
        updatedVisited :: visitedNeighbors

    dfsHelper startNode []

    
let part1 fn () =
    let input = readInput fn
    let valves = input |> Seq.map (reToValve >> fun v -> (v.Name, v)) |> Map.ofSeq
    printfn "%A" valves
    valves |> Map.values |> Seq.head |> dfs valves |> List.map List.rev |> printfn "%A"
    // let key = List.head paths
    0L

let part2 fn () = 0L
