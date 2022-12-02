module AoC2022.Day2

open AoC2022.Utils

let day2 fn () =
    let rounds = readInput fn

    let score =
        function
        | "A X" -> 3 + 1 // R d R
        | "A Y" -> 6 + 2 // P w R
        | "A Z" -> 0 + 3 // S l R
        | "B X" -> 0 + 1 // R l P
        | "B Y" -> 3 + 2 // P d P
        | "B Z" -> 6 + 3 // S w P
        | "C X" -> 6 + 1 // R w S
        | "C Y" -> 0 + 2 // P l S
        | "C Z" -> 3 + 3 // S d S

    rounds |> Seq.sumBy score |> int64

let day2part2 fn () =
    let rounds = readInput fn

    let score =
        function
        | "A X" -> 0 + 3 // S l R
        | "A Y" -> 3 + 1 // R d R
        | "A Z" -> 6 + 2 // P w R
        | "B X" -> 0 + 1 // R l P
        | "B Y" -> 3 + 2 // P d P
        | "B Z" -> 6 + 3 // S w P
        | "C X" -> 0 + 2 // P l S
        | "C Y" -> 3 + 3 // S d S
        | "C Z" -> 6 + 1 // R w S

    rounds |> Seq.sumBy score |> int64
