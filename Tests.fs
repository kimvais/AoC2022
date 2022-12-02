module AoC2022.Tests


open FsUnit.Xunit
open Xunit

open AoC2022.Utils
open AoC2022.Day1
open AoC2022.Day2
open AoC2022.Day3

[<Fact>]
let ``day 1, part 1`` () =
    day1 "test1" () |> should equal 24000L
    day1 "1" () |> should equal 68775L

[<Fact>]
let ``day1, part2`` () =
    day1part2 "test1" () |> should equal 45000L
    day1part2 "1" () |> should equal 202585L

[<Fact>]
let ``day2, part 1`` () =
    day2 "test2" () |> should equal 15L
    day2 "2" () |> should equal 13526L

[<Fact>]
let ``day2, part 2`` () =
    day2part2 "test2" () |> should equal 12L
    day2part2 "2" () |> should equal 14204L

[<Fact>]
let ``day3`` () =
    day3 "test3" () |> should equal -1L
    day3 "3" () |> should equal -1L

[<Fact>]
let ``day3, part 2`` =
    day3part2 "test3" () |> should equal -1L
    day3part2 "3" () |> should equal -1L
