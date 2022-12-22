module AoC2022.Day22

open System.Text.RegularExpressions
open AoC2022.Utils

type Turn =
    | Left
    | Right

type Instruction = int * Turn

type Direction =
    | East
    | South
    | West
    | North

type Standing = { Facing: Direction; X: int; Y: int }


let makeInstruction (n, dir) =
    match dir with
    | "R" -> n, Right
    | "L" -> n, Left

let instructionRe = Regex @"(\d+)([RL])"

let notSpace (_, c) = c <> ' '

let (convert: seq<seq<char>> -> Map<int, char>[]) =
    Seq.map (Seq.mapi (fun i c -> i, c) >> Seq.filter notSpace >> Map.ofSeq)
    >> Array.ofSeq

let part1 fn () =
    let input = readInput fn

    let mapInput =
        input |> Seq.takeWhile (fun l -> l <> "") |> List.ofSeq

    let instructionsInput = input |> Seq.last

    let instructions =
        instructionRe.Matches instructionsInput
        |> Seq.map (fun m -> (int m.Groups[1].Value, m.Groups[2].Value) |> makeInstruction)

    let mapWidth = mapInput |> Seq.map String.length |> Seq.max

    let rows' =
        mapInput
        |> Seq.map (fun s -> s.PadRight mapWidth |> Seq.map char)


    let cols = rows' |> Seq.transpose |> convert

    let rows = rows' |> convert

    printfn "%A" rows
    printfn "%A" cols
    printfn "%A" instructions

    let start =
        { Facing = East
          X = (rows |> Seq.head |> Map.keys |> Seq.min)
          Y = 0 }

    printfn $"%A{start}"
    0L

let part2 fn () = 0L
