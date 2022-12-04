module AoC2022.Day4

open AoC2022.Utils

let parse (lines: string seq) =
    lines
    |> Seq.map (fun s ->
        s
        |> splitS "[-,]"
        |> Array.map int
        |> fun arr -> [ arr[0] .. arr[1] ] |> Set.ofList, [ arr[2] .. arr[3] ] |> Set.ofList)

let oneContainsAnother (ass1, ass2) = Set.isSubset ass1 ass2 || Set.isSubset ass2 ass1

let overlaps (ass1, ass2) = Set.intersect ass1 ass2 |> Set.isEmpty |> not

let solveDay4 matcher fn () =
    readInput fn
    |> parse
    |> Seq.filter matcher
    |> Seq.length
    |> int64

let day4 = solveDay4 oneContainsAnother
let day4part2 = solveDay4 overlaps
