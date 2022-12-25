module AoC2022.Day25

open AoC2022.Utils
open System

let powersOf5 = Seq.initInfinite (fun i -> pown 5L i)

let snafuDigit = function
    | '2' -> 2L
    | '1' -> 1L
    | '0' -> 0L
    | '-' -> -1L
    | '=' -> -2L

let snafuNumber = function
    | 0L -> "0"
    | 1L -> "1"
    | 2L -> "2"
    | -2L -> "="
    | -1L -> "-"
   
let deSnafu s =
    s |> Seq.rev |> Seq.map snafuDigit |> Seq.zip powersOf5 |> Seq.map (fun (a,b) -> a * b) |> Seq.sum

let snafu (num:int64) =
    let unfolder n =
        match n with
        | 0L -> None
        | _ ->
            match n % 5L with
            | 4L -> Some (-1L, ((n + 1L) / 5L))
            | 3L -> Some (-2L, ((n + 2L) / 5L))
            | x -> Some (x, (n / 5L))
    Seq.unfold unfolder num |> Seq.map (snafuNumber >> string) |> Seq.rev |> String.concat ""
    
let part1 fn () =
    let input = readInput fn
    input |> Seq.map deSnafu |> Seq.sum |> snafu |> printfn "%s"
    0L
    