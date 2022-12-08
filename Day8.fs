module AoC2022.Day8

open AoC2022.Utils

let getForest fn =
    let input = readInput fn

    let trees =
        input
        |> Array.ofSeq
        |> Array.map (Array.ofSeq >> Array.map (string >> int))

    let transposed = trees |> Array.transpose
    let maxX = Array.length trees
    let maxY = Array.length (Array.head trees)
    trees, transposed, maxX, maxY

let part1 fn () =

    let trees, transposed, maxX, maxY = getForest fn

    let getTallest =
        function
        | [||] -> -1
        | a -> Array.max a

    let innerTrees =
        seq {
            for y in [ 0 .. maxX - 1 ] do
                for x in [ 0 .. maxY - 1 ] do
                    let tree = trees.[y][x]
                    let west = trees.[y][0 .. x - 1] |> getTallest
                    let east = trees.[y][x + 1 .. maxX] |> getTallest
                    let north = transposed.[x][0 .. y - 1] |> getTallest
                    let south = transposed.[x][y + 1 .. maxY] |> getTallest
                    let los = [ north; west; east; south ] |> List.min
                    yield los < tree
        }
        |> Seq.filter id
        |> Seq.length
        |> int64

    innerTrees


let part2 fn () =

    let trees, transposed, maxX, maxY = getForest fn

    seq {
        for y in [ 1 .. maxX - 2 ] do
            for x in [ 1 .. maxY - 2 ] do
                let tree = trees.[y][x]
                let west = trees.[y][0 .. x - 1] |> Array.rev
                let east = trees.[y][x + 1 .. maxX]
                let north = transposed.[x][0 .. y - 1] |> Array.rev
                let south = transposed.[x][y + 1 .. maxY]

                let los =
                    [ north; west; east; south ]
                    |> List.map (Seq.takeWhilePlus1 (fun h -> h < tree) >> Seq.length >> int64)

                yield los |> List.reduce (*)
    }
    |> Seq.max
