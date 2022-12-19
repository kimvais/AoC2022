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
      Round: int }

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
    | 25 -> state
    | _ ->
        let needOreBots =
            Seq.max [ bp.Ore.Cost; bp.Clay.Cost; bp.Obsidian.Cost; bp.Geode.Cost ]

        let needClayBots =
            float bp.Obsidian.ClayCost / float bp.Obsidian.Cost
            |> ceil
            |> int

        let needObisidianBots =
            float bp.Geode.ObsidianCost / float bp.Geode.Cost |> ceil |> int

        printfn $"Bot targets - Ore: %d{needOreBots}, Clay: %d{needClayBots}, Obsidian: %d{needObisidianBots} "

        let newState =
            { state with
                Round = state.Round + 1
                Ore = state.Ore + state.OreBots
                Clay = state.Clay + state.ClayBots
                Obsidian = state.Obsidian + state.ObsidianBots
                Geodes = state.Geodes + state.GeodeBots }

        let state' =
            if state.Obsidian >= bp.Geode.ObsidianCost && state.Ore >= bp.Geode.Cost then
                { newState with
                    GeodeBots = newState.GeodeBots + 1
                    Ore = newState.Ore - bp.Geode.Cost
                    Obsidian = newState.Obsidian - bp.Geode.ObsidianCost }
            elif
                state.ObsidianBots < needObisidianBots && state.Ore >= bp.Obsidian.Cost
                && state.Clay >= bp.Obsidian.ClayCost
            then
                { newState with
                    ObsidianBots = newState.ObsidianBots + 1
                    Ore = newState.Ore - bp.Obsidian.Cost
                    Clay = newState.Clay - bp.Obsidian.ClayCost }
            elif state.ClayBots < needClayBots && state.Ore >= bp.Clay.Cost then
                { newState with
                    ClayBots = newState.ClayBots + 1
                    Ore = newState.Ore - bp.Clay.Cost }
            elif state.OreBots < needOreBots && state.Ore >= bp.Ore.Cost then
                { newState with
                    OreBots = newState.OreBots + 1
                    Ore = newState.Ore - bp.Ore.Cost }
            else
                newState

        mine bp state'

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
          Round = 0 }

    blueprints |> Seq.map (fun bp -> mine bp initialState) |> printfn "%A"
    0L

let part2 fn () =
    parse fn
    0L
