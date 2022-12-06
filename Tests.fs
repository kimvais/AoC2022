module AoC2022.Tests


open FsUnit.Xunit
open Xunit

open AoC2022.Utils
open AoC2022.Day1
open AoC2022.Day2
open AoC2022.Day3
open AoC2022.Day4
open AoC2022.Day5
open AoC2022.Day6

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
let ``day3, part 1`` () =
    day3 "test3" () |> should equal 157L
    day3 "3" () |> should equal 7674L

[<Fact>]
let ``day3, part 2`` =
    day3part2 "test3" () |> should equal 70L
    day3part2 "3" () |> should equal 2805L

[<Fact>]
let ``day4, part 1`` () =
    day4 "test4" () |> should equal 2L 
    day4 "4" () |> should equal 536L 

[<Fact>]
let ``day4, part 2`` () =
    day4part2 "test4" () |> should equal 4L
    day4part2 "4" () |> should equal 845L

[<Fact>]
let ``day5, part 1`` () =
    solveDay5 move "test5" () |> should equal "CMZ"
    solveDay5 move "5" () |> should equal "RNZLFZSJH"

[<Fact>]
let ``day5, part2`` () =
    solveDay5 move9001 "test5" () |> should equal "MCD"
    solveDay5 move9001 "5" () |> should equal "CNSFCGJSM"
    
[<Fact>]
let ``day6, part 1`` () =
    findMarker 4 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" |> should equal 7L
    findMarker 4 "bvwbjplbgvbhsrlpgdmjqwftvncz" |> should equal 5L
    findMarker 4 "nppdvjthqldpwncqszvftbrmjlhg" |> should equal 6L
    findMarker 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" |> should equal 10L
    findMarker 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" |> should equal 11L
    day6 "6" () |> should equal 1531L
    
[<Fact>]
let ``day6, part 2`` () =
    findMarker 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" |> should equal 19L
    findMarker 14 "bvwbjplbgvbhsrlpgdmjqwftvncz" |> should equal 23L
    findMarker 14 "nppdvjthqldpwncqszvftbrmjlhg" |> should equal 23L
    findMarker 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" |> should equal 29L
    findMarker 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" |> should equal 26L
    day6part2 "6" () |> should equal 2518L
