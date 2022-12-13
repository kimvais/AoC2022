module AoC2022.Day13

open System.Text.Json.Nodes
open AoC2022.Utils
open FSharp.Data
open FSharp.Data.Runtime.BaseTypes

type Packet =
    | Int of int
    | List of Packet list

let parseList json = JsonValue.Parse(json)

let rec parse json =
    match json with
    | JsonValue.Number n -> Int(int n)
    | JsonValue.Array a -> (a |> List.ofArray) |> List.map parse |> List

let parseRecord r =
    let pair = r |> splitByLinefeed
    let left = parseList (Seq.head pair) |> parse
    let right = parseList (Seq.last pair) |> parse
    match left, right with 
    | List l, List r -> l, r
    | _ -> failwith "Error: one of the lists is not a list!"

let part1 fn () =
    let input = readInputDelimByEmptyLine fn
    let records = input |> Array.map parseRecord
    printfn "%A" records
    0L

let part2 fn () = 0L
