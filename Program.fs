module AoC2022.Main

open AoC2022.Utils
open AoC2022.Day1

[<EntryPoint>]
let main argv =
    let day = argv |> getProblem
    match day with
    | "1" -> day1 "1" ()
    | "1b" -> day1part2 "1" ()
    | "test" -> day1 "test" ()
    |> printfn "%d"
    
    0
    