module AoC2022.Day14

open AoC2022.Utils

let parseCoords (s: string) =
    s.Split(',')
    |> Array.map int
    |> (fun a -> Seq.head a, Seq.last a)

let makeBlocks ((xa: int, ya: int), (xb: int, yb: int)) =
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

let rec fall v blocks x y =
    match y with
    | n when n >= v -> true, blocks
    | _ -> 
        // printfn $"Falling at %d{x},%d{y}"
        match Set.contains (x, y + 1) blocks with
        | false -> fall v blocks x (y + 1)
        | true ->
            match Set.contains (x - 1, y + 1) blocks with
            | false -> fall v blocks (x - 1) (y + 1)
            | true ->
                match Set.contains (x + 1, y + 1) blocks with
                | false -> fall v blocks (x + 1) (y + 1)
                | true ->
                    // printfn $"Fell at %d{x},%d{y}"
                    (false, Set.add (x, y) blocks)

let rec drop voidStartsAt blocks =
    let result, newBlocks = fall voidStartsAt blocks 500 0
    match result with
    | true -> blocks 
    | false -> drop voidStartsAt newBlocks 
    
let part1 fn () =
    let lines = parseInput fn
    let blocks = lines |> Set.ofSeq
    let voidStartsAt = lines |> Seq.maxBy (snd >> (+) 1) |> snd

    // printfn "%A" lines
    // printfn $"Void: %i{voidStartsAt}"
    let blockCount = Set.count blocks
    // printfn "%d" blockCount
    drop voidStartsAt blocks |> Set.count |> (+) -blockCount |> int64

let part2 fn () = 0L
