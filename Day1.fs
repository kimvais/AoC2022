module AoC2022.Day1

open AoC2022.Utils

let countCalories input =
    let elves =

        input
        |> splitByTwoLinefeeds
        |> Seq.map (fun e -> e |> splitByLinefeed |> Seq.map int64)

    elves |> Seq.map (fun f -> f |> Seq.sum)


let part1 fn () =
    let input = readInput fn |> String.concat "\n"
    countCalories input |> Seq.max

let part2 fn () =
    let input = readInput fn |> String.concat "\n"
    countCalories input |> Seq.sortDescending |> Seq.take 3 |> Seq.sum
