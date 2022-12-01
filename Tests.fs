module AoC2022.Tests


open FsUnit.Xunit
open Xunit

open AoC2022.Utils
open AoC2022.Day1

[<Fact>]
let ``day 1, part 1`` () =
    day1 "test1" () |> should equal 24000L
    day1 "1" () |> should equal 68775L
    
[<Fact>]
let ``day1, part2`` () =
    day1part2 "test1" () |> should equal 45000L
    day1part2 "1" () |> should equal 202585L
