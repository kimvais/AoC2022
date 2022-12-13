module AoC2022.Day13

open System.Text.Json.Nodes
open AoC2022.Utils
open FSharp.Data
open FSharp.Data.Runtime.BaseTypes

type Packet =
    | Int of int
    | PacketList of Packet
    
type NestedArrays = JsonProvider<""" [ [1, 2, 3], [4, 5, [6, 7]], [8, 9, 10, [11, 12, 13, 14]] ] """>

let parseList json = JsonValue.Parse(json)

let rec parse json =
    match json with
    | JsonValue.Number n -> Int (int n)
    | JsonValue.Array a -> (a |> List.ofArray) |> List.map parse |> PacketList
    
let parseRecord r =
    let pair = r |> splitByLinefeed
    let left = (parseList ("[" + (Seq.head pair) + "]"))[0]
    let right = (parseList ("[" + (Seq.last pair) + "]"))[0]
    printfn $"%A{left} %A{right}"
    (parse left), right

let part1 fn () =
    let input = readInputDelimByEmptyLine fn
    let records = input |> Array.map parseRecord
    let left, right = records |> Seq.last
    printfn "%A" left
    0L

let part2 fn () = 0L
