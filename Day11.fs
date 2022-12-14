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

let toSecond (n: int64) = pown (int n) 2 |> int64
let modPowWithMagic (magic: int64) (n:int64) =
    let b = bigint n
    BigInteger.ModPow(b, 2I, (bigint magic)) |> int64
let divBy3 (n: int64) = n / 3L

let doOperation monkey =
    match monkey.Op with
    | RaiseToSecondPower -> toSecond >> divBy3
    | Add n -> (+) n >> divBy3
    | Multiply n -> (*) n >> divBy3

let doOperation2 magic monkey =
    match monkey.Op with
    |RaiseToSecondPower -> modPowWithMagic magic
    | Add n -> (+) n >> ((%) magic)
    | Multiply n -> (*) n >> ((%) magic)
    
let getToss monkey item =
    match item % monkey.Divisor with
    | 0L -> (monkey.TrueTarget, item)
    | _ -> (monkey.FalseTarget, item)

let monkeyDo magic monkey =
    let op = match magic with
        | None -> doOperation monkey
        | Some m -> doOperation2 m monkey

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

let rec doRound divFunc monkeys inTurn =
    match (monkeys |> Map.keys |> Seq.max) - inTurn with
    | -1 -> monkeys
    | _ ->
        let monkey = monkeys.[inTurn]
        let inspections = monkey.Inspections
        let tosses = monkey |> monkeyDo divFunc

        let monkeys' =
            monkeys
            |> Map.add
                inTurn
                { monkey with
                    Items = Array.empty
                    Inspections = inspections + (Seq.length monkey.Items |> int64) }
            |> Map.map (toss tosses)

        doRound divFunc monkeys' (inTurn + 1)

let rec play divFunc monkeys rounds =
    match rounds with
    | 0 -> monkeys
    | n -> play divFunc (doRound divFunc monkeys 0) (n - 1)

let solve divFunc rounds monkeys  =

    play divFunc monkeys rounds 
    |> Map.values
    |> Seq.map (fun m -> m.Inspections)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (*)

let moduloMagic magic n =
    n % magic
    
let part1 fn () =
    let input = readInputDelimByEmptyLine fn
    let monkeys = input |> Array.map parseMonkey |> Map.ofArray
    solve None  20 monkeys

let part2 fn () =
    let input = readInputDelimByEmptyLine fn
    let monkeys = input |> Array.map parseMonkey |> Map.ofArray
    let magic = monkeys |> Map.values |> Seq.map (fun m -> m.Divisor) |> Seq.reduce (*) 
    solve (Some magic) 10000 monkeys
