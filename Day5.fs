module AoC2022.Day5

open AoC2022.Utils

let parseInstruction s =
    let fields = s |> splitS " " |> Array.ofSeq

    [ fields.[1]; fields.[3]; fields.[5] ]
    |> List.map int
    |> (fun a -> a.[0], a.[1] - 1, a.[2] - 1)

let rec move source destination count (positions: char list array) =
    match count with
    | 0 -> positions
    | n ->
        let src = positions.[source]
        let dst = positions.[destination]
        let src' = List.tail src
        let dst' = [ (List.head src) ] @ dst
        positions.[source] <- src'
        positions.[destination] <- dst'
        move source destination (n - 1) positions

let rec moveCrates instructions positions =
    match instructions with
    | i when Seq.isEmpty i -> positions
    | i ->
        let count, source, destination = Seq.head i
        let positions' = move source destination count positions
        moveCrates (Seq.tail instructions) positions'

let day5 fn () =
    let input = readInput fn |> String.concat "\n" |> splitByTwoLinefeeds
    let crates = Seq.head input |> splitByLinefeed

    let instructions =
        Seq.last input |> splitByLinefeed |> Seq.map parseInstruction

    let positions =
        crates
        |> Seq.map (Seq.chunkBySize 4)
        |> Seq.transpose
        |> Seq.map (
            Seq.rev
            >> Seq.map (Seq.skip 1 >> Seq.head)
            >> Seq.takeWhile (fun c -> c <> ' ')
            >> Seq.tail
            >> Seq.rev
            >> List.ofSeq
        )
        |> Array.ofSeq

    let finalPositions = moveCrates instructions positions

    finalPositions
    |> Array.map List.head
    |> Array.map string
    |> Array.reduce (+)
    |> printfn "%s"

    0L

let day5part2 fn () = 0L
