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

type State =
    { Ore: int
      OreBots: int
      Clay: int
      ClayBots: int
      Obsidian: int
      ObsidianBots: int
      Geodes: int
      GeodeBots: int
      TimeLeft: int }

let parse fn =
    let robotRe =
        Regex
            @"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."

    let input = readInput fn

    let parseLine line =
        let m = robotRe.Match line

        let values =
            [ for g in (m.Groups |> Seq.tail) -> int g.Value ] |> Array.ofSeq

        { Id = values.[0]
          Ore = { Cost = values.[1] }
          Clay = { Cost = values.[2] }
          Obsidian = { Cost = values.[3]; ClayCost = values.[4] }
          Geode = { Cost = values.[5]; ObsidianCost = values.[6] } }

    input |> Seq.map parseLine

let solve minutes blueprints =
    let rec mine (bp: Blueprint) s =

        if s.TimeLeft = 0 then
            s
        else
            let minute = s.TimeLeft - 1

            let res =
                seq
                    [ yield
                          mine
                              bp
                              { s with
                                  TimeLeft = minute
                                  Ore = s.Ore + s.OreBots
                                  Clay = s.Clay + s.ClayBots
                                  Obsidian = s.Obsidian + s.ObsidianBots
                                  Geodes = s.Geodes + s.GeodeBots }

                      if
                          s.Ore >= bp.Ore.Cost
                          && s.OreBots < (Seq.max [ bp.Ore.Cost; bp.Clay.Cost; bp.Obsidian.Cost; bp.Geode.Cost ])
                      then
                          yield
                              mine
                                  bp
                                  { s with
                                      TimeLeft = minute
                                      Ore = s.Ore + s.OreBots - bp.Ore.Cost
                                      Clay = s.Clay + s.ClayBots
                                      Obsidian = s.Obsidian + s.ObsidianBots
                                      Geodes = s.Geodes + s.GeodeBots
                                      OreBots = s.OreBots + 1 }

                      if s.Ore >= bp.Clay.Cost && s.ClayBots < bp.Obsidian.ClayCost then
                          yield
                              mine
                                  bp
                                  { s with
                                      TimeLeft = minute
                                      Ore = s.Ore + s.OreBots - bp.Clay.Cost
                                      Clay = s.Clay + s.ClayBots
                                      Obsidian = s.Obsidian + s.ObsidianBots
                                      Geodes = s.Geodes + s.GeodeBots
                                      ClayBots = s.ClayBots + 1 }

                      if s.Ore >= bp.Obsidian.Cost && s.Clay >= bp.Obsidian.ClayCost then
                          yield
                              mine
                                  bp
                                  { s with
                                      TimeLeft = minute
                                      Ore = s.Ore + s.OreBots - bp.Obsidian.Cost
                                      Clay = s.Clay + s.ClayBots - bp.Obsidian.ClayCost
                                      Obsidian = s.Obsidian + s.ObsidianBots
                                      Geodes = s.Geodes + s.GeodeBots
                                      ObsidianBots = s.ObsidianBots + 1 }

                      if s.Ore >= bp.Geode.Cost && s.Obsidian >= bp.Geode.ObsidianCost then
                          yield
                              mine
                                  bp
                                  { s with
                                      TimeLeft = minute
                                      Ore = s.Ore + s.OreBots - bp.Geode.Cost
                                      Clay = s.Clay + s.ClayBots
                                      Obsidian = s.Obsidian + s.ObsidianBots - bp.Geode.ObsidianCost
                                      Geodes = s.Geodes + s.GeodeBots
                                      GeodeBots = s.GeodeBots + 1 } ]

            res |> Seq.maxBy (fun s -> s.Geodes)


    blueprints
    |> Seq.map (fun bp ->
        bp.Id,
        mine
            bp
            { TimeLeft = minutes
              Ore = 0
              Clay = 0
              Obsidian = 0
              Geodes = 0
              OreBots = 1
              ClayBots = 0
              ObsidianBots = 0
              GeodeBots = 0 })

let part1 fn () =
    let blueprints = parse fn

    blueprints
    |> solve 24
    |> Seq.sumBy (fun (i, s) -> i * s.Geodes)
    |> int64

let part2 fn () =
    let blueprints = parse fn |> Seq.take 3

    blueprints
    |> solve 32
    |> Seq.map (fun (_, s) -> s.Geodes)
    |> Seq.reduce (*)
    |> int64
