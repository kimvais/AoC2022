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

let rec move9001 source destination count (positions: char list array) =
    let src = positions.[source]
    let dst = positions.[destination]
    let src' = src |> List.skip count
    let dst' = (src |> List.take count) @ dst
    positions.[source] <- src'
    positions.[destination] <- dst'
    positions

let rec moveCrates moveFunc instructions positions =
    match instructions with
    | i when Seq.isEmpty i -> positions
    | i ->
        let count, source, destination = Seq.head i
        let positions' = moveFunc source destination count positions
        moveCrates moveFunc (Seq.tail instructions) positions'

let getInput fn =
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

    positions, instructions

let getOutput positions =
    positions
    |> Array.map List.head
    |> Array.map string
    |> Array.reduce (+)

let solve func fn () =
    let positions, instructions = getInput fn
    moveCrates func instructions positions |> getOutput

let part1 fn () =
    solve move fn () |> printfn "%s"
    0L

let part2 fn () =
    solve move9001 fn () |> printfn "%s"
    0L
