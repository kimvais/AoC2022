module AoC2022.Day3

open AoC2022.Utils

let parseLine line =
    line
    |> Seq.map (function
        | c when c >= 'a' && c <= 'z' -> int64 c - int64 'a' + 1L
        | c when c >= 'A' && c <= 'Z' -> int64 c - int64 'A' + 27L
        | c -> failwith "Invalid character %A" char c)

let day3 fn () =
    let input = readInput fn

    let getCommon rucksacks =
        let rucksacks' = rucksacks |> Seq.map Set.ofSeq
        let r1 = Seq.head rucksacks'
        let r2 = Seq.last rucksacks'
        Set.intersect r1 r2 |> Set.toSeq |> Seq.exactlyOne

    input
    |> Seq.map (parseLine >> Seq.splitInto 2 >> getCommon)
    |> Seq.sum

let day3part2 fn () =
    let input = readInput fn

    input
    |> Seq.map (parseLine >> Set.ofSeq)
    |> Seq.chunkBySize 3
    |> Seq.map (Set.intersectMany >> Set.toSeq >> Seq.exactlyOne)
    |> Seq.sum
