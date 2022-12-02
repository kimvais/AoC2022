module AoC2022.Main

open AoC2022.Utils
open AoC2022.Day1
open AoC2022.Day2

[<EntryPoint>]
let main argv =
    let day = argv |> getProblem
    match day with
    | "1" -> day1 "1" ()
    | "1b" -> day1part2 "1" ()
    | "2" -> day2 "2" ()
    | "2b" -> day2part2 "2" ()
    | "test" -> day2 "2" ()
    |> printfn "%d"
    
    0
    