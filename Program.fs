module AoC2022.Main

open AoC2022.Utils
open AoC2022

[<EntryPoint>]
let main argv =
    let day = argv |> getProblem

    match day with
    | "1" -> Day1.part1 "1" ()
    | "1b" -> Day1.part2 "1" ()
    | "2" -> Day2.part1 "2" ()
    | "2b" -> Day2.part2 "2" ()
    | "3" -> Day3.part1 "3" ()
    | "3b" -> Day3.part2 "3" ()
    | "4" -> Day4.part1 "4" ()
    | "4b" -> Day4.part2 "4" ()
    | "5" -> Day5.part1 "5" ()
    | "5b" -> Day5.part2 "5" ()
    | "6" -> Day6.part1 "6" ()
    | "6b" -> Day6.part2 "6" ()
    | "7" -> Day7.part1 "7" ()
    | "7b" -> Day7.part2 "7" ()
    | "test" -> Day7.part1 "test7" ()
    |> printfn "%d"

    0
