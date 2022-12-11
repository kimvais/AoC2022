module AoC2022.Day11

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
      FalseTarget: int }

let parseMonkey (lines: string) =
    let [| idLine; itemsLine; opLine; testLine; trueLine; falseLine |] =
        lines.Split '\n'

    let monkeyId =
        idLine.Replace(":", "").Split(' ') |> Seq.last |> int

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
    let monkey = {
        Id= monkeyId
        Items= items
        Op=operation
        Divisor=test
        TrueTarget=ifTrue
        FalseTarget=ifFalse
    }
    printfn "%A" monkey
    monkey

let part1 fn () =
    let input = readInputDelimByEmptyLine fn
    let monkeys = input |> Array.map parseMonkey
    
    0L


let part2 fn () = 0L
