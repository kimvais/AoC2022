module AoC2022.Day15

open System.Text.RegularExpressions
open AoC2022.Utils

let getDistance vec =
    let (sX, sY), (bX, bY) = vec
    let xd = abs (sX - bX)
    let yd = abs (sY - bY)
    xd, yd

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

let findSensorsWithinRange row sensors = sensors |> Seq.filter (fun s -> (abs (s.Y - row) <= s.Range))

let getCoverage coords rowNo =
    let sensors =
        coords
        |> Seq.map (fun c -> (fst c, (getDistance >> getRange) <| c) |> makeSensor)

    let withInRange = sensors |> findSensorsWithinRange rowNo

    withInRange
    |> Seq.map (fun s -> s.X, s.Range - (abs (s.Y - rowNo)))
    |> Seq.map (fun (x, range) -> (x - range, x + range))

let part1 rowNo fn () =
    let coords = parse fn
    let coverage = getCoverage coords rowNo
    printfn $"%A{coverage |> List.ofSeq}"
    let first = coverage |> Seq.minBy fst |> fst
    let last = coverage |> Seq.maxBy snd |> snd
    printfn $"%A{first} %A{last}"
    last - first

let folder (s: int64, e: int64, gap: int64 option) ((l1: int64, r1: int64), (l2: int64, r2: int64)) =
    let s' = Seq.min [ s; l1; l2 ]

    let gap' =
        match l2 - r1 with
        | n when n >= 0L ->
            match gap with
            | Some s -> Some s
            | None -> Some r1
        | _ -> None

    s', r2, gap'

let findGap (ranges: (int64 * int64) seq) : int64 * int64 * int64 option =
    let ranges' = ranges |> Seq.pairwise
    ranges' |> Seq.fold folder (0, 20L, None)
   
let rec gapFinder (n:int64) (rem: (int64 * int64) list) =
    match rem with
    | [] -> None
    | l ->
        let candidates  = l |> List.sortBy fst |> List.takeWhile (fun t -> (fst t) <= n) |> List.sortByDescending snd
        match candidates with
        | [] -> Some n
        | c ->
            let next = c |> List.head |> snd
            let rem' = l |> List.filter (fun t -> (fst t) >= next)
            printfn $"%d{next} %A{rem'}"
            gapFinder next rem'
        

let part2 fn () =
    let coords = parse fn
    (*
    let coverage =
        [ 0L .. 20L ]
        |> Seq.map (fun cov -> getCoverage coords cov |> findGap)

    coverage
    |> Seq.map (fun (_, _, x) -> x)
    |> List.ofSeq
    |> printfn "%A"
*)
    [0L..20L] |> Seq.map (
        fun y -> 
        let line = (getCoverage coords y |> List.ofSeq)
        printfn $"%A{line}"
        gapFinder 0L line
    ) |> List.ofSeq |> printfn "%A"
    0L