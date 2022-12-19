module AoC2022.Day18

open AoC2022.Utils

let getNeighbours x y z =
    seq
        [ x - 1, y, z
          x + 1, y, z
          x, y - 1, z
          x, y + 1, z
          x, y, z - 1
          x, y, z + 1 ]

let part1 fn () =
    let input = readInput fn

    let cubes =
        input
        |> Array.ofSeq
        |> Array.map (fun r -> r.Split(',') |> Array.map int)

    cubes |> printfn "%A"
    let blocks = cubes |> Array.map (fun [| x; y; z |] -> x, y, z)
    let lavaBlob = blocks |> Set.ofArray

    blocks
    |> Array.map (fun (x, y, z) ->
        getNeighbours x y z
        |> Seq.filter (fun c -> Set.contains c lavaBlob) |> Seq.length |> (-) 6)
    |> Seq.sum
    |> int64


let part2 fn () = 
    let input = readInput fn

    let cubes =
        input
        |> Array.ofSeq
        |> Array.map (fun r -> r.Split(',') |> Array.map int)
    let [| xs; ys; zs |] = cubes |> Array.transpose
    let minX = Array.min xs
    let maxX = Array.max xs
    let minY = Array.min ys
    let maxY = Array.max ys
    let minZ = Array.min zs
    let maxZ = Array.max zs
    printfn $"x: %d{minX}-%d{maxX}, y: %d{minY}-%d{maxY}, z: %d{minZ}-%d{maxZ}"
    let blocks = cubes |> Array.map (fun [| x; y; z |] -> x, y, z)
    let lavaBlob = blocks |> Set.ofArray
    for x in minX .. maxX do
        for y in minY .. maxY do
            for z in minZ .. maxZ do
                ()
    0L