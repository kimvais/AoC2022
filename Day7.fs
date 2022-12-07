module AoC2022.Day7

open System.Text.RegularExpressions
open AoC2022.Utils

let (|Regex|_|) pattern s =
    let m = Regex.Match(s, pattern)

    match m.Success with
    | false -> None
    | true -> Some(List.tail [ for g in m.Groups -> g.Value ])

let changeFunc num value =
    match value with
    | None -> Some(int64 num)
    | Some x -> Some(num + int64 x)

let parse (paths, dirs: Map<string list, int64>) line =
    match line with
    | Regex @"\$ cd \.\." [] -> List.tail paths, dirs
    | Regex @"\$ cd (.*)" [ dirName ] -> [ dirName ] @ paths, dirs
    | Regex @"([0-9]+) .*" [ fileSize ] ->
        let rec traverse (tail: string list) directories =
            match tail with
            | [] -> directories
            | _ :: t ->
                let newDirs =
                    Map.change tail (changeFunc (int64 fileSize)) directories

                traverse t newDirs

        paths, traverse paths dirs
    | _ -> paths, dirs

let getDirectorySizes fn =
    readInput fn
    |> Seq.fold parse ([], Map.empty<string list, int64>)
    |> snd
    |> Map.values
    
let part1 fn () =
    getDirectorySizes fn
    |> Seq.filter ((>) 100000)
    |> Seq.sum

let part2 fn () =
    let dirSizes = getDirectorySizes fn
    let total = 70_000_000L
    let needed = 30_000_000L
    let toBeFreed = needed - (total - (Seq.max dirSizes))
    dirSizes |> Seq.filter ((<) toBeFreed) |> Seq.min
