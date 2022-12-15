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

let part1 rowNo fn () =
    let coords = parse fn
    // printfn $"%A{coords}"
    // let distances = coords |> Seq.map getDistance
    // printfn $"%A{distances}"

    let sensors =
        coords
        |> Seq.map (fun c -> (fst c, (getDistance >> getRange) <| c) |> makeSensor)

    let withInRange = sensors |> findSensorsWithinRange rowNo
    // printfn $"%A{sensors}"
    // printfn $"%A{withInRange |> List.ofSeq}"

    let coverage =
        withInRange
        |> Seq.map (fun s -> s.X, s.Range - (abs (s.Y - rowNo)))
        |> Seq.map (fun (x, range) -> [ x - range; x + range ])
        |> Seq.sort 

    // let beaconsOnRow = coords |> Seq.map snd |> Seq.filter (fun (_,y) -> y = rowNo) |> Set.ofSeq |> Set.count |> int64
    // printfn $"%d{beaconsOnRow}"
    printfn $"%A{coverage |> List.ofSeq}"
    let first = coverage |> Seq.minBy Seq.head |> Seq.head
    let last = coverage |> Seq.maxBy Seq.last |> Seq.last
    // coverage |> Set.count |> int64 |> (+) -beaconsOnRow
    printfn $"%A{first} %A{last}"
    last - first
    
let part2 fn () = 0L
