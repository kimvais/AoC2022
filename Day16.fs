module AoC2022.Day16

open System.Text.RegularExpressions
open AoC2022.Utils

type Valve = { Id: string; FlowRate: int64; Neighbours: string list }

let valveRe =
    Regex @"^Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z, ]+)"

let reToValve (s) =
    let m = valveRe.Match(s)
    let neighbours = m.Groups.[3].Value |> splitS ", " |> List.ofArray

    let valve =
        { Id = m.Groups.[1].Value
          FlowRate = m.Groups.[2].Value |> int64
          Neighbours = neighbours }

    printfn $"%A{valve}"
    valve

let part1 fn () =
    let input = readInput fn
    let valves = input |> Seq.map reToValve
    valves |> Seq.sumBy (fun v -> v.FlowRate)

let part2 fn () = 0L
