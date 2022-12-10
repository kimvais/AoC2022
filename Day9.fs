module AoC2022.Day9

open AoC2022.Utils

type State = { Hx: int; Hy: int; Tx: int; Ty: int; Visited: Set<int * int> }

let getDirection = fst >>
    function
    | "R" -> (0, 1)
    | "U" -> (-1, 0)
    | "L" -> (0, -1)
    | "D" -> (1, 0)
    | _ -> failwith "Invalid instruction"

let moveHead instruction =
    let dir = getDirection instruction
    Seq.replicate (snd instruction) dir

let getDistance state =
    state.Hy - state.Ty, state.Hx - state.Tx

let move state (dY, dX) =
    let hy = state.Hy + dY
    let hx = state.Hx + dX
    let state' = { state with Hy = hy; Hx = hx }

    let ty, tx =
        match getDistance state' with
        | 2, 0 -> state.Ty + 1, state.Tx
        | -2, 0 -> state.Ty - 1, state.Tx
        | 0, 2 -> state.Ty, state.Tx + 1
        | 0, -2 -> state.Ty, state.Tx - 1
        | 2, 1 | 1, 2 -> state.Ty + 1, state.Tx + 1
        | -2, 1 | -1, 2 -> state.Ty - 1, state.Tx + 1
        | 1, -2 | 2, -1 -> state.Ty + 1, state.Tx - 1
        | -2, -1 | -1, -2 -> state.Ty - 1, state.Tx - 1
        | _ -> state.Ty, state.Tx

    { state' with
        Ty = ty
        Tx = tx
        Visited = Set.add (ty, tx) state.Visited }

let part1 fn () =
    let input =
        readInput fn
        |> Seq.map (fun s -> s.Split ' ' |> fun [| a; b |] -> (a, int b))

    let initialState =
        { Hx = 0
          Hy = 0
          Tx = 0
          Ty = 0
          Visited = Set.singleton (0, 0) }

    let moves = input |> Seq.map moveHead |> Seq.concat
    let state = moves |> Seq.fold move initialState
    state.Visited |> Set.count |> int64

let part2 fn () = 0L
