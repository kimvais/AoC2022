module AoC2022.Day6

open AoC2022.Utils

let findMarker sampleSize s =
    (s
     |> Seq.windowed sampleSize
     |> Seq.takeWhile (fun chars -> chars |> Set.ofSeq |> Set.count < sampleSize)
     |> Seq.length)
    + sampleSize
    |> int64

let solveDay6 sampleSize fn =
    let input = readInput fn
    input |> Seq.head |> findMarker sampleSize

let part1 fn () = solveDay6 4 fn

let part2 fn () = solveDay6 14 fn
