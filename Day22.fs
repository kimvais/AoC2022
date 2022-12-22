module AoC2022.Day22

open System.Text.RegularExpressions
open AoC2022.Utils

type Direction =
    | Left
    | Right

type Instruction =
    | Left of int
    | Right of int

let makeInstruction (n, dir) =
    match dir with
    | "R" -> Right n
    | "L" -> Left n

let instructionRe = Regex @"(\d+)([RL])"

let part1 fn () =
    let input = readInput fn

    let mapInput =
        input |> Seq.takeWhile (fun l -> l <> "") |> List.ofSeq

    let instructionsInput = input |> Seq.last

    let instructions =
        instructionRe.Matches instructionsInput
        |> Seq.map (fun m -> (int m.Groups[1].Value, m.Groups[2].Value) |> makeInstruction)

    let mapWidth = mapInput |> Seq.map String.length |> Seq.max

    let rows =
        mapInput
        |> Seq.map (fun s -> s.PadRight mapWidth |> Seq.map char)

    let notSpace (_, c) = c <> ' '

    let cols =
        rows
        |> Seq.transpose
        |> Seq.map (Seq.mapi (fun i c -> i, c) >> Seq.filter notSpace)

    let rows' =
        rows
        |> Seq.map (Seq.mapi (fun i c -> i, c) >> Seq.filter notSpace)

    printfn "%A" rows'
    printfn "%A" cols
    printfn "%A" instructions
    0L

let part2 fn () = 0L
