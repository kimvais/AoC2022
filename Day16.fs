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
        match List.length visited with
        | n when n >= 29 -> [ visited ]
        | _ ->
            let neighbors = currentNode.Neighbours
            let updatedVisited = currentNode.Name :: visited

            let unvisitedNeighbors =
                neighbors
                |> List.filter (fun neighbor -> not (List.contains (Map.find neighbor valveMap).Name visited))

            let visitedNeighbors =
                unvisitedNeighbors
                |> List.map (fun neighbor -> dfsHelper (Map.find neighbor valveMap) updatedVisited)
                |> List.concat

            updatedVisited :: visitedNeighbors

    dfsHelper startNode []

let removePrefixes paths =
    let checkIfNotPrefix l1 l2 = Seq.zip l1 l2 |> Seq.exists (fun (a, b) -> a <> b)

    paths
    |> List.filter (fun p ->
        let longer =
            paths
            |> List.sortBy List.length
            |> List.skipWhile (fun l -> List.length l <= List.length p)

        longer |> Seq.forall (fun p' -> checkIfNotPrefix p' p))

let rec letItFlow (valves: Map<string,Valve>) flowed flowRate path secondsRemaining =
    match secondsRemaining = 0 with 
    | true -> flowed
    | false ->
        match Seq.isEmpty path with
        | false -> 
            let valveId = Seq.head path
            let path' = Seq.tail path
            match valves.[valveId].FlowRate with
            | 0L ->
                let flowed' = flowed + flowRate
                letItFlow valves flowed' flowRate path' (secondsRemaining - 1)
            | n ->
                let flowed' = flowed + flowRate * 2L + n
                letItFlow valves flowed' (flowRate + n) path' (secondsRemaining - 2)
        | true -> flowed + flowRate * int64 secondsRemaining
        
    
let part1 fn () =
    let input = readInput fn

    let valves =
        input |> Seq.map (reToValve >> fun v -> (v.Name, v)) |> Map.ofSeq

    printfn "%A" valves

    let aa = valves.["AA"]
    let allPaths =
        aa |> dfs valves |> List.map List.rev |> removePrefixes
         // let key = List.head paths
    // allPaths |> Seq.map (fun p -> letItFlow valves 0L 0L p 30) |> Seq.max
    printfn "%A" allPaths
    allPaths |> Seq.maxBy List.length |> (fun p -> letItFlow valves 0L 0L p 30) 

let part2 fn () = 0L
