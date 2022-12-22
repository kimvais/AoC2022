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

let rec step (line: Map<int, char>) count pos =

    match count with
    | 0 -> pos
    | _ ->
        let idx, change =
            match pos.Facing with
            | East -> pos.X, +1
            | West -> pos.X, -1
            | North -> pos.Y, -1
            | South -> pos.Y, +1

        let newidx =
            match line |> Map.containsKey (idx + change), change with
            | true, c -> idx + c
            | false, -1 -> line |> Map.keys |> Seq.max
            | false, 1 -> line |> Map.keys |> Seq.min

        match line.[newidx], pos.Facing with
        | '.', East
        | '.', West -> step line (count - 1) { pos with X = newidx }
        | '.', North
        | '.', South -> step line (count - 1) { pos with Y = newidx }
        | '#', _ -> pos
        | _ -> failwith "Invalid character in map"

let turn dir pos =
    match dir, pos.Facing with
    | Left, East -> { pos with Facing = North }
    | Left, South -> { pos with Facing = East }
    | Left, West -> { pos with Facing = South }
    | Left, North -> { pos with Facing = West }
    | Right, East -> { pos with Facing = South }
    | Right, South -> { pos with Facing = West }
    | Right, West -> { pos with Facing = North }
    | Right, North -> { pos with Facing = East }

let move (rows: Map<int, char>[]) (cols: Map<int, char>[]) dir count pos =
    let pos' =
        match pos.Facing with
        | East -> step rows.[pos.Y] count pos
        | West -> step rows.[pos.Y] count pos
        | South -> step cols.[pos.X] count pos
        | North -> step cols.[pos.X] count pos

    turn dir pos'

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

    let start =
        { Facing = East
          X = (rows |> Seq.head |> Map.keys |> Seq.min)
          Y = 0 }

    let move' = move rows cols

    let final =
        instructions |> Seq.fold (fun s (c, d) -> move' d c s) start

    let row = final.Y + 1 |> int64
    let col = final.X + 1 |> int64

    let face =
        match final.Facing with
        | East -> 0L
        | South -> 1L
        | West -> 2L
        | North -> 3L

    (row * 1000L) + (col * 4L) + face

let part2 fn () = 0L
