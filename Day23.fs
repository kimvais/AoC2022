module AoC2022.Day23

open AoC2022.Utils
type Elf = { X: int; Y: int }

type Direction =
    | N
    | S
    | W
    | E

let directions = [ N; S; W; E ] |> Seq.repeatForever

let getNeighbours x y d =
    match d with
    | N -> [ (x - 1, y - 1); (x, y - 1); (x + 1, y - 1) ]
    | S -> [ (x - 1, y + 1); (x, y + 1); (x + 1, y + 1) ]
    | E -> [ (x + 1, y - 1); (x + 1, y); (x + 1, y + 1) ]
    | W -> [ (x - 1, y - 1); (x - 1, y); (x - 1, y + 1) ]

let parse fn =
    let rows = readInput fn

    seq {
        rows
        |> Seq.mapi (fun y r ->
            r
            |> Seq.mapi (fun x c ->
                match c with
                | '#' -> Some(x, y)
                | '.' -> None
                | _ -> failwith "Invalid character"

            ))
    }
    |> Seq.concat
    |> Seq.concat
    |> Seq.choose id

let part1 fn () =
    let elves = parse fn
    printfn $"%A{elves}"
    0L

let part2 fn () = 0L
