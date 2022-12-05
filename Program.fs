module AoC2022.Main

open AoC2022.Utils
open AoC2022.Day1
open AoC2022.Day2
open AoC2022.Day3
open AoC2022.Day4
open AoC2022.Day5
open AoC2022.Day6

[<EntryPoint>]
let main argv =
    let day = argv |> getProblem
    match day with
    | "1" -> day1 "1" ()
    | "1b" -> day1part2 "1" ()
    | "2" -> day2 "2" ()
    | "2b" -> day2part2 "2" ()
    | "3" -> day3 "3" ()
    | "3b" -> day3part2 "3" ()
    | "4" -> day4 "4" ()
    | "4b" -> day4part2 "4" ()
    | "5" -> day5 "5" ()
    | "5b" -> day5part2 "5" ()
    | "6" -> day6 "6" ()
    | "6b" -> day6part2 "6" ()
    | "test" -> day6 "test6" ()
    |> printfn "%d"
    
    0
    