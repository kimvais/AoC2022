module AoC2022.Day20

open AoC2022.Utils

type Number = { OrigPos: int64; Value: int64 }

let printNumbers n = n |> List.map (fun n -> n.Value) |> printfn "%A"

let rec mix numbers toMix =
    let len = List.length numbers - 1 |> int64

    match toMix with
    | [] -> numbers
    | l ->
        let h = List.head l
        let pos = numbers |> List.findIndex (fun n -> n = h) |> int64
        let front, back = numbers |> List.splitAt (int pos)
        let numbers' = front @ (List.tail back)

        match len with
        | 6L ->
            printfn "%d at %d to %d" h.Value pos (pos + h.Value)
            printNumbers numbers
        | _ -> ()

        let insertAt = (2L * len + pos + h.Value) % len

        mix (numbers' |> List.insertAt (int insertAt) h) (List.tail l)

let solve fn key rounds =
    let numbers =
        readInput fn
        |> List.ofSeq
        |> List.mapi (fun i s -> { OrigPos = i; Value = int64 s  })

    let toMix = numbers |> List.sortBy (fun n -> n.OrigPos)

    let mixed =
        mix numbers toMix
        |> Seq.repeatForever
        |> Seq.skipWhile (fun n -> n.Value <> 0)

    [ 1000; 2000; 3000 ]
    |> Seq.map (fun v -> mixed |> Seq.skip v |> Seq.head |> (fun n -> n.Value))
    |> Seq.sum
    |> int64

let part1 fn () =
    solve fn 1L 1L
let part2 fn () = 0L
