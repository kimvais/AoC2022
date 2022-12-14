module AoC2022.Tests


open FsUnit.Xunit
open Xunit

open AoC2022

[<Fact>]
let ``day 1, part 1`` () =
    Day1.part1 "test1" () |> should equal 24000L
    Day1.part1 "1" () |> should equal 68775L

[<Fact>]
let ``day1, part2`` () =
    Day1.part2 "test1" () |> should equal 45000L
    Day1.part2 "1" () |> should equal 202585L

[<Fact>]
let ``day2, part 1`` () =
    Day2.part1 "test2" () |> should equal 15L
    Day2.part1 "2" () |> should equal 13526L

[<Fact>]
let ``day2, part 2`` () =
    Day2.part2 "test2" () |> should equal 12L
    Day2.part2 "2" () |> should equal 14204L

[<Fact>]
let ``day3, part 1`` () =
    Day3.part1 "test3" () |> should equal 157L
    Day3.part1 "3" () |> should equal 7674L

[<Fact>]
let ``day3, part 2`` =
    Day3.part2 "test3" () |> should equal 70L
    Day3.part2 "3" () |> should equal 2805L

[<Fact>]
let ``day4, part 1`` () =
    Day4.part1 "test4" () |> should equal 2L
    Day4.part1 "4" () |> should equal 536L

[<Fact>]
let ``day4, part 2`` () =
    Day4.part2 "test4" () |> should equal 4L
    Day4.part2 "4" () |> should equal 845L

[<Fact>]
let ``day5, part 1`` () =
    Day5.solve Day5.move "test5" () |> should equal "CMZ"
    Day5.solve Day5.move "5" () |> should equal "RNZLFZSJH"

[<Fact>]
let ``day5, part2`` () =
    Day5.solve Day5.move9001 "test5" () |> should equal "MCD"
    Day5.solve Day5.move9001 "5" () |> should equal "CNSFCGJSM"

[<Fact>]
let ``day6, part 1`` () =
    Day6.findMarker 4 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    |> should equal 7L

    Day6.findMarker 4 "bvwbjplbgvbhsrlpgdmjqwftvncz"
    |> should equal 5L

    Day6.findMarker 4 "nppdvjthqldpwncqszvftbrmjlhg"
    |> should equal 6L

    Day6.findMarker 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    |> should equal 10L

    Day6.findMarker 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    |> should equal 11L

    Day6.part1 "6" () |> should equal 1531L

[<Fact>]
let ``day6, part 2`` () =
    Day6.findMarker 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    |> should equal 19L

    Day6.findMarker 14 "bvwbjplbgvbhsrlpgdmjqwftvncz"
    |> should equal 23L

    Day6.findMarker 14 "nppdvjthqldpwncqszvftbrmjlhg"
    |> should equal 23L

    Day6.findMarker 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    |> should equal 29L

    Day6.findMarker 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    |> should equal 26L

    Day6.part2 "6" () |> should equal 2518L

[<Fact>]
let ``day 7, part 1`` () =
    Day7.part1 "test7" () |> should equal 95437L
    Day7.part1 "7" () |> should equal 1517599L

[<Fact>]
let ``day 7, part 2`` () =
    Day7.part2 "test7" () |> should equal 24933642L
    Day7.part2 "7" () |> should equal 2481982L

[<Fact>]
let ``day 8, part 1`` () =
    Day8.part1 "test8" () |> should equal 21L
    Day8.part1 "8" () |> should equal 1820L

[<Fact>]
let ``day 8, part 2`` () =
    Day8.part2 "test8" () |> should equal 8L
    Day8.part2 "8" () |> should equal 385112L

[<Fact>]
let ``day 9, part 1`` () =
    Day9.part1 "test9" () |> should equal 13L
    // Day9.part2 "9" () |> should equal 
   
[<Fact>]
let ``day 9, part 2`` () =
    // Day9.part2 "test9" () |> should equal 
    // Day9.part2 "9" () |> should equal 
    ()

[<Fact>]
let ``day 10, part 1`` () =
    Day10.part1 "test10" () |> should equal 13140L
    
[<Fact>]
let ``day 11, part 1`` () =
    Day11.part1 "test11" () |> should equal 10605L
    Day11.part1 "11" () |> should equal 182293L

[<Fact>]
let ``day 11, part 2`` () =
    Day11.part2 "test11" () |> should equal 2713310158L
    
[<Fact>]
let ``day 12, part 1`` () =
    Day12.part1 "test12" () |> should equal 31L
    Day12.part1 "12" () |> should equal 408L
    
[<Fact>]
let ``day 12, part 2`` () =
    Day12.part2 "test12" () |> should equal 29L
    Day12.part2 "12" () |> should equal 399L
   
[<Fact>]
let ``day 13, part 1`` fn () =
    Day13.part1 "test13" |> should equal 13L
    Day13.part2 "14" |> should equal 5292L
    
[<Fact>]
let ``day 13, part 2`` fn () =
    Day13.part2 "test13" |> should equal 140L
    