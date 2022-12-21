module AoC2022.Day21

open System
open AoC2022.Utils


type Operation =
    | Add
    | Sub
    | Mul
    | Div

type Monkey =
    | NumberMonkey of int64
    | OperationMonkey of Operation * string * string

type HalfMonkey =
    | Int of int64
    | Source of string

let parseOp =
    function
    | "*" -> Mul
    | "+" -> Add
    | "-" -> Sub
    | "/" -> Div

let parse line =
    match line with
    | Regex @"^([a-z]{4}): (\d+)" [ mId; n ] -> mId, NumberMonkey(int64 n)
    | Regex @"^([a-z]{4}): ([a-z]{4}) (.) ([a-z]{4})" [ mId; mA; op; mB ] -> mId, OperationMonkey(parseOp op, mA, mB)

let calculate op (a: int64) (b: int64) =
    match op with
    | Add -> (+) a b
    | Mul -> (*) a b
    | Sub -> (-) a b
    | Div -> (/) a b

let revOp op (a: int64) (b: int64) =
    match op with
    | Sub -> (+) a b
    | Div -> (*) a b
    | Add -> (-) a b
    | Mul -> (/) a b

let revOpName =
    function
    | Div -> "*"
    | Add -> "-"
    | Mul -> "/"
    | Sub -> "+"

let doMonkey (numberMonkeys: Map<string, Monkey>) opMonkey =
    let n =
        match opMonkey with
        | OperationMonkey (o, ma, mb) ->
            let (NumberMonkey a) = numberMonkeys.[ma]
            let (NumberMonkey b) = numberMonkeys.[mb]
            calculate o a b
        | _ -> failwith "Invalid monkey"

    NumberMonkey n

let solvePart2 numberMonkeys opMonkeys =
    let monkeyChain =
        opMonkeys
        |> Seq.map (fun (i, m) ->
            let (OperationMonkey (op, ma, mb)) = m
            let am = numberMonkeys |> Map.tryFind ma
            let bm = numberMonkeys |> Map.tryFind mb
            (*
            
            match am with
            | None -> ()
            | Some x -> printfn $"%A{op} %A{x} %A{mb}"

            match bm with
            | None -> ()
            | Some x -> printfn $"%A{op} %A{ma} %A{x}"
            *)
            match am, bm with
            | Some (NumberMonkey n), None -> i, (op, Source mb, Int n)
            | None, Some (NumberMonkey n) -> i, (op, Int n, Source ma) )
    let monkeys = monkeyChain |> Map.ofSeq
    printfn "%A" monkeys
    let _, final, next = monkeys.["root"]

    let rec unChain (chain: Map<string, Operation * int64 * string>) acc next =
        match next with
        | "humn" -> acc
        | _ ->
            let op, n, next' = chain.[next]
            printfn $"%A{acc} %s{revOpName op} %A{n} ="
            unChain chain ((revOp op) acc n) next'
    0L
    
    // unChain monkeys final next

let rec shout (numberMonkeys: Map<string, Monkey>) (opMonkeys: Set<string * Monkey>) =

    let readyMonkeys =
        opMonkeys
        |> Set.filter (fun (_, m) ->
            match m with
            | NumberMonkey _ -> false
            | OperationMonkey (_, a, b) ->
                Map.containsKey a numberMonkeys
                && Map.containsKey b numberMonkeys)

    // readyMonkeys |> printfn "%A"
    let opMonkeys' = Set.difference opMonkeys readyMonkeys

    let newNumberMonkeys =
        readyMonkeys
        |> Set.map (fun (i, m) -> i, doMonkey numberMonkeys m)
        |> Map.ofSeq

    let numberMonkeys' =
        Map.fold (fun acc k v -> Map.add k v acc) numberMonkeys newNumberMonkeys

    match readyMonkeys |> Set.isEmpty with
    | true ->
        match Map.tryFind "root" numberMonkeys' with
        | Some (NumberMonkey m) -> m
        | None -> solvePart2 numberMonkeys' opMonkeys'
    | false -> shout numberMonkeys' opMonkeys'


let getMonkeys fn =
    let input = readInput fn |> Seq.map parse

    let numberMonkeys =
        input
        |> Seq.filter (fun (_, m) ->
            match m with
            | NumberMonkey _ -> true
            | _ -> false)
        |> Map.ofSeq

    let opMonkeys =
        input
        |> Seq.filter (fun (_, m) ->
            match m with
            | OperationMonkey _ -> true
            | _ -> false)
        |> Set.ofSeq

    numberMonkeys, opMonkeys

let part1 fn () =
    let numberMonkeys, opMonkeys = getMonkeys fn
    shout numberMonkeys opMonkeys |> int64

let part2 fn () =
    let numberMonkeys, opMonkeys = getMonkeys fn
    let numberMonkeys' = numberMonkeys |> Map.remove "humn"
    shout numberMonkeys' opMonkeys |> int64
