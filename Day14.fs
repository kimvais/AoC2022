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

let rec fall finishCondition isBlocked blocks x y =
    let isFinished = finishCondition blocks x y

    match isFinished with
    | true -> true, blocks
    | false ->
        // printfn $"Falling at %d{x},%d{y}"
        match isBlocked blocks x (y + 1) with
        | false -> fall finishCondition isBlocked blocks x (y + 1)
        | true ->
            match isBlocked blocks (x - 1) (y + 1) with
            | false -> fall finishCondition isBlocked blocks (x - 1) (y + 1)
            | true ->
                match isBlocked blocks (x + 1) (y + 1) with
                | false -> fall finishCondition isBlocked blocks (x + 1) (y + 1)
                | true ->
                    // printfn $"Fell at %d{x},%d{y}"
                    (false, Set.add (x, y) blocks)

let rec drop finishCondition isBlocked blocks =
    let result, newBlocks = fall finishCondition isBlocked blocks 500 0

    match result with
    | true -> blocks
    | false -> drop finishCondition isBlocked newBlocks

let part1 fn () =
    let lines = parseInput fn
    let blocks = lines |> Set.ofSeq
    let voidStartsAt = lines |> Seq.maxBy (snd >> (+) 1) |> snd
    let finishCondition _ _ y = y >= voidStartsAt
    let isBlocked b x y  = Set.contains (x, y) b

    let blockCount = Set.count blocks
    drop finishCondition isBlocked blocks
    |> Set.count
    |> (+) -blockCount
    |> int64

let part2 fn () = 
    let lines = parseInput fn
    let blocks = lines |> Set.ofSeq
    let floor = lines |> Seq.maxBy snd |> snd |> (+) 2
    printfn $"Floor: %i{floor}"
    let finishCondition b _ _ = Set.contains (500, 0) b
    let isBlocked b x y = Set.contains (x, y) b || y >= floor
    let blockCount = Set.count blocks
    
    drop finishCondition isBlocked blocks
    |> Set.count
    |> (+) -blockCount
    |> int64