module AoC2022.Day9

open AoC2022.Utils

type State = { Hx: int; Hy: int; Tx: int; Ty: int; Visited: Set<int * int> }

let getDirection =
    function
    | "R", n -> (0, 1)
    | "U", n -> (-1, 0)
    | "L", n -> (0, -1)
    | "D", n -> (1, 0)
    | _ -> failwith "Invalid instruction"

let moveHead instruction =
    let dir = getDirection instruction
    Seq.replicate (snd instruction) dir

let getDistance state =
    // printfn $"Distances: %d{dY}, %d{dX}"
    state.Ty - state.Hy, state.Tx - state.Hx

let move state (dY,dX) = 
    let y = state.Hy + dY
    let x = state.Hx + dX
    { state with Hy = y; Hx = x; Visited = Set.add (y,x) state.Visited }

let part1 fn () =
    let input =
        readInput fn
        |> Seq.map (fun s -> s.Split ' ' |> fun [| a; b |] -> (a, int b))

    printfn "%A" input

    let initialState =
        { Hx = 0
          Hy = 0
          Tx = 0
          Ty = 0
          Visited = Set.singleton (0, 0) }

    let moves = input |> Seq.map moveHead |> Seq.concat 
    let state = moves |> Seq.fold move initialState
    printfn "%A" state
    state.Visited |> Set.count |> int64

let part2 fn () = 0L
