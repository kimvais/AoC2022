module AoC2022.Day11

open System.Numerics
open AoC2022.Utils

type Operation =
    | Add of int64
    | Multiply of int64
    | RaiseToSecondPower

type Monkey =
    { Id: int
      Items: int64 array
      Op: Operation
      Divisor: int64
      TrueTarget: int
      FalseTarget: int
      Inspections: int64 }

// let magic = 96577L
let magic = 9699690L

let parseMonkey (lines: string) =
    let [| idLine; itemsLine; opLine; testLine; trueLine; falseLine |] =
        lines.Split '\n'

    let monkeyId = idLine.Replace(":", "").Split(' ') |> Seq.last |> int

    let items =
        itemsLine.Replace(",", "")
        |> splitS " "
        |> Array.skip 4
        |> Array.map int64

    let operation =
        opLine.Split(" ")
        |> Array.skip 6
        |> function
            | [| "*"; "old" |] -> RaiseToSecondPower
            | [| "*"; n |] -> Multiply(int64 n)
            | [| "+"; n |] -> Add(int64 n)
            | _ -> failwith "Invalid operation"

    let test = testLine.Split(' ') |> Seq.last |> int64
    let ifTrue = trueLine.Split(' ') |> Seq.last |> int
    let ifFalse = falseLine.Split(' ') |> Seq.last |> int

    let monkey =
        { Id = monkeyId
          Items = items
          Op = operation
          Divisor = test
          TrueTarget = ifTrue
          FalseTarget = ifFalse
          Inspections = 0L }

    monkeyId, monkey

let pow2 n = pown (int n) 2 |> int64
let toSecond (n: int64) =
    let b = bigint n
    BigInteger.ModPow(b, 2I, magic) |> int64
    
let divBy3 (n: int64) = n / 3L

let doOperation isPart2 monkey =
    match isPart2 with
    | true ->
        match monkey.Op with
        | RaiseToSecondPower -> toSecond 
        | Add n -> (+) n 
        | Multiply n -> (*) n 
    | false ->
        match monkey.Op with
        | RaiseToSecondPower -> pow2 >> divBy3
        | Add n -> (+) n >> divBy3
        | Multiply n -> (*) n >> divBy3

let getToss monkey item =
    match item % monkey.Divisor with
    | 0L -> (monkey.TrueTarget, item)
    | _ -> (monkey.FalseTarget, item)

let monkeyDo isPart2 () monkey  =
    let op = doOperation isPart2 monkey

    let newItems =
        monkey.Items
        |> Array.map (op >> getToss monkey)
        |> Array.groupBy fst
        |> Array.map (fun (target, tosses) -> target, tosses |> Array.map snd)
        |> Map.ofArray

    newItems

let toss (tosses: Map<int, int64 array>) (monkeyId: int) (monkey: Monkey) =
    match tosses |> Map.tryFind monkeyId with
    | None -> monkey
    | Some items -> { monkey with Items = Array.append monkey.Items items }

let rec doRound isPart2 monkeys inTurn =
    match (monkeys |> Map.keys |> Seq.max) - inTurn with
    | -1 -> monkeys
    | _ ->
        let monkey = monkeys.[inTurn]
        let inspections = monkey.Inspections
        let tosses = monkey |> monkeyDo isPart2 ()

        let monkeys' =
            monkeys
            |> Map.add
                inTurn
                { monkey with
                    Items = Array.empty
                    Inspections = inspections + (Seq.length monkey.Items |> int64) }
            |> Map.map (toss tosses)

        doRound isPart2 monkeys' (inTurn + 1)

let rec play isPart2 monkeys rounds =
    match rounds with
    | 0 -> monkeys
    | n -> play isPart2 (doRound isPart2 monkeys 0) (n - 1)

let solve isPart2 rounds fn  =
    let input = readInputDelimByEmptyLine fn
    let monkeys = input |> Array.map parseMonkey |> Map.ofArray
    let magic = monkeys |> Map.values |> Seq.map (fun m -> m.Divisor) |> Seq.reduce (*)
    printfn $"%d{magic}"
    let manageWorryLevel i =
        i % magic
    play isPart2 monkeys rounds 
    |> Map.values
    |> Seq.map (fun m -> m.Inspections)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (*)

let part1 fn () = solve false 20 fn

let part2 fn () = solve true 10000 fn
