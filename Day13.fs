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
    left, right

let rec compare left right =
    match left, right with
    | Int l, Int r -> l.CompareTo r
    | Int _, List _ -> compare (List [ left ]) right
    | List _, Int _ -> compare left (List [ right ])
    | List l, List r ->
        Seq.zip l r
        |> Seq.map (fun (a, b) -> compare a b)
        |> Seq.skipWhile ((=) 0)
        |> Seq.tryHead
        |> function
            | Some h -> h
            | None -> (Seq.length l).CompareTo(Seq.length r)

let part1 fn () =
    let input = readInputDelimByEmptyLine fn

    input
    |> Array.map parseRecord
    |> Array.mapi (fun i (l, r) -> (i + 1, compare l r))
    |> Array.filter (snd >> (=) -1)
    |> Array.sumBy fst
    |> int64

let compareSnd (_, a) (_, b) = compare a b

let part2 fn () =
    let input =
        (readInput fn |> Seq.filter (fun s -> s <> ""))
        |> Array.ofSeq
        |> Array.map (fun a -> false, a)
        |> Array.append [| (true, "[[2]]")
                           (true, "[[6]]") |]

    let packets =
        input
        |> Array.map (fun (i, p) -> i, (p |> parseList |> parse))
        |> Array.sortWith compareSnd

    packets
    |> Array.mapi (fun i (isDivider, _) ->
        match isDivider with
        | true -> i + 1
        | false -> 1)
    |> Array.reduce (*)
    |> int64
