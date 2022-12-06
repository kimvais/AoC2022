module AoC2022.Day2

open AoC2022.Utils

let part1 fn () =
    let rounds = readInput fn

    let score =
        function
        | "A X" -> 3 + 1 // Rock draws with Rock
        | "A Y" -> 6 + 2 // Paper wins Rock
        | "A Z" -> 0 + 3 // Scissors loses to Rock
        | "B X" -> 0 + 1 // Rock loses to Paper
        | "B Y" -> 3 + 2 // Paper draws with Paper
        | "B Z" -> 6 + 3 // Scissors wins Paper
        | "C X" -> 6 + 1 // Rock wins Scissors
        | "C Y" -> 0 + 2 // Paper loses to Scissors
        | "C Z" -> 3 + 3 // Scissors draws with Scissors

    rounds |> Seq.sumBy score |> int64

let part2 fn () =
    let rounds = readInput fn

    let score =
        function
        | "A X" -> 0 + 3 // Scissors loses to Rock
        | "A Y" -> 3 + 1 // Rock draws with Rock
        | "A Z" -> 6 + 2 // Paper wins Rock
        | "B X" -> 0 + 1 // Rock loses to Paper
        | "B Y" -> 3 + 2 // Paper draws with Paper
        | "B Z" -> 6 + 3 // Scissors wins Paper
        | "C X" -> 0 + 2 // Paper loses to Scissors
        | "C Y" -> 3 + 3 // Scissors draws with Scissors
        | "C Z" -> 6 + 1 // Rock wins Scissors

    rounds |> Seq.sumBy score |> int64
