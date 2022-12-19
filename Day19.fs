module AoC2022.Day19

open System.Text.RegularExpressions
open AoC2022.Utils

let minutes = 24

type Material =
    | Ore
    | Clay
    | Obsidian

type OreBot = { Cost: int }

type ClayBot = { Cost: int }

type ObsidianBot = { Cost: int; ClayCost: int }

type GeodeBot = { Cost: int; ObsidianCost: int }

type Robot =
    | OreBot
    | ClayBot
    | ObsidianBot
    | GeodeBot

type Blueprint =
    { Id: int
      Ore: OreBot
      Clay: ClayBot
      Obsidian: ObsidianBot
      Geode: GeodeBot }

let parse fn =
    let robotRe =
        Regex
            @"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."

    let input = readInput fn

    let parseLine line =
        let m = robotRe.Match line
        let values = [ for g in (m.Groups |> Seq.tail) -> int g.Value ] |> Array.ofSeq

        { Id = values.[0]
          Ore = { Cost = values.[1] }
          Clay = { Cost = values.[2] }
          Obsidian = { Cost = values.[3]; ClayCost = values.[4] }
          Geode = { Cost = values.[5]; ObsidianCost = values.[6] } }

    input |> Seq.map parseLine |> printfn "%A"

let part1 fn () =
    parse fn
    0L
    
let part2 fn () =
    parse fn
    0L
