module AoC2022.Day4

open AoC2022.Utils

let parse (lines: string seq) =
    let makeSet assignment =
        let (a1, a2), (b1, b2) = assignment
        let ass1 = [ a1..a2 ] |> Set.ofSeq
        let ass2 = [ b1..b2 ] |> Set.ofSeq
        ass1, ass2

    let assignments =
        lines
        |> Seq.map (fun s ->
            s.Split ','
            |> Seq.map (fun s' ->
                s'.Split '-'
                |> Seq.map int64
                |> (fun s'' -> (Seq.head s'', Seq.last s'')))
            |> (fun s''' -> (Seq.head s''', Seq.last s''')))

    assignments |> Seq.map makeSet

let oneContainsAnother (ass1, ass2) = Set.isSubset ass1 ass2 || Set.isSubset ass2 ass1

let overlaps (ass1, ass2) = Set.intersect ass1 ass2 |> Set.isEmpty |> not

let solveDay4 matcher fn () =
    readInput fn |> parse |> Seq.filter matcher |> Seq.length |> int64

let day4 = solveDay4 oneContainsAnother
let day4part2 = solveDay4 overlaps
