module AoC2022.Day15

open System.Text.RegularExpressions
open AoC2022.Utils
open FSharp.Collections.ParallelSeq

let getDistance ((sX, sY), (bX, bY)) =
    let x1 = Seq.max [ sX; bX ]
    let x2 = Seq.min [ sX; bX ]
    let y1 = Seq.max [ sY; bY ]
    let y2 = Seq.min [ sY; bY ]

    let addToX =
        match sign x1, sign x2 with
        | -1, 1
        | 1, -1 -> 1L
        | _ -> 0L

    let addToY =
        match sign y1, sign y2 with
        | -1, 1
        | 1, -1 -> 1L
        | _ -> 0L

    let xd = x1 - x2
    let yd = y1 - y2
    xd + addToX, yd + addToY

let getRange (xd, yd) = xd + yd

let rowRe =
    Regex(@"^Sensor at x=(\-?\d+), y=(\-?\d+): closest beacon is at x=(\-?\d+), y=(\-?\d+)$")

type Sensor = { X: int64; Y: int64; Range: int64 }

let makeSensor ((x, y), r) = { X = x; Y = y; Range = r }

let parse fn =
    readInput fn
    |> Seq.map (fun l ->
        let m = rowRe.Match(l)

        [| m.Groups.[1]; m.Groups.[2]; m.Groups.[3]; m.Groups.[4] |]
        |> Array.map (fun g -> g.Value |> int64))
    |> Seq.map (fun a -> (a.[0], a.[1]), (a.[2], a.[3]))
    |> List.ofSeq

let findSensorsWithinRange row sensors = sensors |> Seq.filter (fun s -> (abs (s.Y - row) <= s.Range))

let getCoverage coords rowNo =
    let sensors =
        coords
        |> Seq.map (fun c -> (fst c, (getDistance >> getRange) <| c) |> makeSensor)


    let withInRange = sensors |> findSensorsWithinRange rowNo

    withInRange
    |> Seq.map (fun s -> s.X, s.Range - (abs (s.Y - rowNo)))
    |> Seq.map (fun (x, range) -> (x - range, x + range))



let rec gapFinder (n: int64) (m: int64) (rem: (int64 * int64) seq) =

    match n with
    | x when x >= m -> None
    | _ ->
        let candidates =
            rem
            |> Seq.filter (fun t -> (fst t) <= n && (snd t) > n)
            |> Seq.sortByDescending snd

        match candidates |> Seq.isEmpty with
        | true -> Some n
        | false ->
            let next = (candidates |> Seq.head |> snd) + 1L
            let rem' = rem |> Seq.filter (fun t -> (snd t) >= next)
            gapFinder next m rem'

let part1 rowNo fn () =
    let coords = parse fn
    let coverage = getCoverage coords rowNo
    let first = coverage |> Seq.minBy fst |> fst
    let last = coverage |> Seq.maxBy snd |> snd

    let total =
        last - first
        // Add sensors on the row to locations.
        + (coords
           |> Seq.map (fun ((x, _), (_, _)) -> x)
           |> Seq.filter (fun n -> n = rowNo)
           |> Seq.length
           |> int64)

    match sign first with
    // We need to fix the addToX for position 0 on distance calculation
    | -1 -> total - 1L
    | _ -> total

let part2 maxV fn () =
    let coords = parse fn

    let position =
        [ 0L .. maxV ]
        |> Seq.map (fun y ->
            let line = getCoverage coords y

            match y % 1000L with
            | 0L -> printfn $"%d{y}"
            | _ -> ()

            gapFinder 0L maxV line)
        |> Seq.mapi (fun i x -> (int64 i, x))
        |> Seq.find (fun (_, v) ->
            match v with
            | None -> false
            | Some _ -> true)

    printfn "%A" position
    let y, x = position
    x.Value * 4000000L + y
