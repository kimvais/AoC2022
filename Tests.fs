module AoC2022.Tests


open FsUnit.Xunit
open Xunit

open AoC2022.Utils
open AoC2022.Day1

[<Fact>]
let ``day 1, part 1`` () =
    day1 "test1" () |> should equal 0L
    day1part2 "test1" () |> should equal 0L
