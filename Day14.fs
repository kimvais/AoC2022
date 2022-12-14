module AoC2022.Day14

open AoC2022.Utils

let parseCoords (s: string) =
    s.Split(',')
    |> Array.map int64
    |> (fun a -> Seq.head a, Seq.last a)

let part1 fn () =
    let input = readInput fn

    let makeBlocks ((xa: int64, ya: int64), (xb: int64, yb: int64)) =
        let x1 = [ xa; xb ] |> Seq.min
        let x2 = [ xa; xb ] |> Seq.max
        let y1 = [ ya; yb ] |> Seq.min
        let y2 = [ ya; yb ] |> Seq.max

        match x1 = x2 with
        | true -> [ y1..y2 ] |> List.map (fun y -> (x1, y))
        | false -> [ x1..x2 ] |> List.map (fun x -> (x, y1))


    let lines =
        input
        |> Seq.map (
            splitS " -> "
            >> List.ofArray
            >> List.map parseCoords
            >> List.pairwise
            >> List.map makeBlocks
            >> List.concat
        )
        |> Seq.concat
        |> Set.ofSeq

    printfn "%A" lines
    printfn "%d" (Set.count lines)
    0L

let part2 fn () = 0L
