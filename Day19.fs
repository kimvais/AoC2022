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
      Round: int
      Depth: int
      }

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

    let bp = input |> Seq.map parseLine
    bp |> printfn "%A"
    bp

let rec mine (bp: Blueprint) state =
    match state.Round with
    | 24 -> state
    | _ ->
        let state' =
            { state with
                Round = state.Round + 1
                Ore = state.Ore + state.OreBots
                Clay = state.Clay + state.ClayBots
                Obsidian = state.Obsidian + state.ObsidianBots
                Geodes = state.Geodes + state.GeodeBots
                Depth = state.Depth + 1}

        seq
            [ yield mine bp { state' with Round = state'.Round + 1 }
              if state.Ore >= bp.Ore.Cost then
                  yield
                      mine
                          bp
                          { state' with
                              Ore = state'.Ore - bp.Ore.Cost
                              OreBots = state.OreBots + 1 }
              if state.Ore >= bp.Clay.Cost then
                  yield
                      mine
                          bp
                          { state' with
                              Ore = state'.Ore - bp.Clay.Cost
                              ClayBots = state.ClayBots + 1 }
              if
                  state.Ore >= bp.Obsidian.Cost
                  && state.Clay >= bp.Obsidian.ClayCost
              then
                  yield
                      mine
                          bp
                          { state' with
                              Ore = state'.Ore - bp.Obsidian.Cost
                              Clay = state'.Clay - bp.Obsidian.ClayCost
                              ObsidianBots = state.ObsidianBots + 1 }
              if
                  state.Ore >= bp.Geode.Cost
                  && state.Obsidian >= bp.Geode.ObsidianCost
              then
                  yield
                      mine
                          bp
                          { state' with
                              Ore = state'.Ore - bp.Geode.Cost
                              Obsidian = state'.Obsidian - bp.Geode.ObsidianCost
                              GeodeBots = state.GeodeBots + 1 } ]
        |> Seq.maxBy (fun s -> s.Geodes) 


let part1 fn () =
    let blueprints = parse fn

    let initialState =
        { Ore = 0
          Clay = 0
          Obsidian = 0
          Geodes = 0
          OreBots = 1
          ClayBots = 0
          ObsidianBots = 0
          GeodeBots = 0
          Round = 1
          Depth = 0
          }

    blueprints
    |> Seq.map (fun bp -> mine bp initialState)
    |> printfn "%A"

    0L

let part2 fn () =
    parse fn
    0L
