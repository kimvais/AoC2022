module AoC2022.Day14

open AoC2022.Utils

let parseCoords (s: string) =
    s.Split(',')
    |> Array.map int64
    |> (fun a -> Seq.head a, Seq.last a)

let makeBlocks ((xa: int64, ya: int64), (xb: int64, yb: int64)) =
    let x1 = [ xa; xb ] |> Seq.min
    let x2 = [ xa; xb ] |> Seq.max
    let y1 = [ ya; yb ] |> Seq.min
    let y2 = [ ya; yb ] |> Seq.max

    match x1 = x2 with
    | true -> [ y1..y2 ] |> List.map (fun y -> (x1, y))
    | false -> [ x1..x2 ] |> List.map (fun x -> (x, y1))


let parseInput fn =
    let input = readInput fn

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

let part1 fn () =
    let lines = parseInput fn
    let blocks = lines |> Set.ofSeq
    let voidRight = lines |> Seq.maxBy (fst >> (+) 1L) |> fst
    let voidLeft = lines |> Seq.minBy (fst >> (+) 1L) |> fst
    
    printfn "%A" lines
    printfn $"Left: %i{voidLeft} - Right: %i{voidRight}"
    printfn "%d" (Set.count blocks)
    0L

let part2 fn () = 0L
