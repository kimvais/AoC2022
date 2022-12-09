module AoC2022.Day9

open AoC2022.Utils

type State = { H: int * int; T: int * int; Visited: Set<int * int> }

let getDirection =
    function
    | "R", n -> (0, n)
    | "U", n -> (-n, 0)
    | "L", n -> (0, -n)
    | "D", n -> (0, n)
    | _ -> failwith "Invalid instruction"

let getDistance state =
    let hy, hx = state.H
    let ty, tx = state.T
    let dY = hy - ty
    let dX = hx - tx
    // printfn $"Distances: %d{dY}, %d{dX}"
    dY, dX

let getTailMove state y x =
    match y, x with
    | 0, n when n > 1 -> (0, 1)
    | 0, n when n < -1 -> (0, -1)
    | n, 0 when n > 1 -> (1, 0)
    | n, 0 when n < -1 -> (-1, 0)
    | a, b when (abs a) <= 1 && (abs b) <= 1 -> (0, 0)
    | a, b when abs a <= 1 -> 0, (b / (abs b))
    | a, b when abs b <= 1 -> (a / (abs a)), 0
    | _ -> failwith (sprintf "%A" state)

let moveTail (dY, dX) (state: State) =
    let x, y = state.T
    let newX = x + dX
    let newY = y + dY
    // printfn $"moving tail to %d{newY} %d{newX}"

    let newVisited = Set.add (newY, newX) state.Visited
    let newState = { state with Visited = newVisited; T = newY, newX }
    // printfn "%A" newState
    newState

let rec catchUp state =
    let dY, dX = getDistance state

    match dY, dX with
    | y, x when abs y < 2 && abs x < 2 -> state
    | y, x ->
        let tailMove = getTailMove state y x
        // printfn $"catching up: %A{tailMove}"
        let newState = moveTail tailMove state
        catchUp newState



let move state instruction =
    printfn $"%A{instruction}"
    let deltaY, deltaX = getDirection instruction
    let y, x = state.H
    let newY = y + deltaY
    let newX = x + deltaX
    printfn $"Y: %d{newY}, X: %d{newX}"
    let newState = { state with H = (newY, newX) }
    catchUp newState

let part1 fn () =
    let input =
        readInput fn
        |> Seq.map (fun s -> s.Split ' ' |> fun [| a; b |] -> (a, int b))

    printfn "%A" input

    let initialState =
        { H = 0, 0; T = 0, 0; Visited = Set.singleton (0, 0) }

    let state = input |> Seq.fold move initialState
    state.Visited |> Set.count |> int64

let part2 fn () = 0L
