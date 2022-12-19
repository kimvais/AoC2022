module AoC2022.Day17

open AoC2022.Utils

type Push =
    | Left
    | Right

type Rock =
    | Bar
    | Cross
    | Corner
    | Pole
    | Square

let sprite =
    function
    | Bar -> [ (0, 0); (0, 1); (0, 2); (0, 3) ]
    | Cross -> [ (0, 1); (1, 0); (1, 2); (2, 1) ] // Middle pixel is not relevant
    | Corner -> [ (2, 0); (2, 1); (2, 2); (1, 2); (0, 2) ]
    | Pole -> [ (0, 0); (1, 0); (2, 0); (3, 0) ]
    | Square -> [ (0, 0); (0, 1); (1, 0); (1, 1) ]

let parseInput s =
    s
    |> Seq.map (function
        | '<' -> Left
        | '>' -> Right)

let getBottomOffset sprite =
    sprite |> Seq.map fst |> Seq.max
    
let getHighest blocks =
    blocks |> Seq.map snd |> Seq.min
    
let getStartPos rock blocks =
    let top = getHighest blocks
    top - getBottomOffset (rock |> sprite) - 4
    
let part1 fn () =
    let input = readInput fn |> Seq.head |> parseInput
    printfn "%A" input
    let winds = input |> Seq.repeatForever
    let blocks = Seq.init 7 (fun i -> (i, 0)) |> Set.ofSeq
    let rocks = seq [Bar; Cross; Corner; Pole; Square] |> Seq.repeatForever
    getStartPos (Seq.head rocks) blocks |> printfn "%A"
    0L

let part2 fn () = 0L