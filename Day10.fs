module AoC2022.Day10

open AoC2022.Utils

type Instruction =
    | AddX of int64
    | NooP

type State = { Cycle: int; X: int64 }

let parse (row: string) =
    match row.Split ' ' with
    | [| "noop" |] -> NooP
    | [| "addx"; n |] -> AddX(int64 n)

let run state instruction =
    match instruction with
    | NooP -> { state with Cycle = state.Cycle + 1 }
    | AddX x -> { state with Cycle = state.Cycle + 2; X = state.X + x }

let getStates fn =
    readInput fn
    |> Seq.map parse
    |> Seq.scan run { Cycle = 1; X = 1 }

let part1 fn () =
    let cycles = [ 20; 60; 100; 140; 180; 220 ]

    let states = getStates fn

    cycles
    |> List.map (fun c ->
        states
        |> Seq.takeWhile (fun s -> s.Cycle <= c)
        |> Seq.last
        |> (fun a -> int64 c, a.X))
    |> Seq.sumBy (fun (c, x) -> c * x)

let rec draw rem state cycle =
    let pos = cycle % 40

    match cycle with
    | 240 -> ()
    | c ->
        let s, r =
            match state.Cycle with
            | n when n >= c -> state, rem
            | _ -> Seq.head rem, Seq.tail rem

        match abs (int s.X - pos) with
        | 0
        | 1 -> printLit ()
        | _ -> printDark ()
        
        if pos = 39 then
            printfn ""

        draw r s (cycle + 1)

let part2 fn () =
    let states = getStates fn
    draw (Seq.tail states) (Seq.head states) 0
    0L
