module AoC2022.Day8

open AoC2022.Utils

let makeForest fn =
    let input = readInput fn

    let forest =
        input
        |> Array.ofSeq
        |> Array.map (Array.ofSeq >> Array.map (string >> int))

    let maxX = Array.length forest
    let maxY = Array.length (Array.head forest)
    forest, maxX, maxY

let getTreesInLineOfSight (forest: int array array) x y maxX maxY =
    let transposed = forest |> Array.transpose
    let north = transposed.[x][0 .. y - 1] |> Array.rev
    let west = forest.[y][0 .. x - 1] |> Array.rev
    let east = forest.[y][x + 1 .. maxX]
    let south = transposed.[x][y + 1 .. maxY]
    seq [ north; west; east; south ]

let part1 fn () =

    let forest, maxX, maxY = makeForest fn

    let getTallest =
        function
        | [||] -> -1
        | a -> Array.max a

    seq {
        for y in [ 0 .. maxX - 1 ] do
            for x in [ 0 .. maxY - 1 ] do
                let tree = forest.[y][x]

                yield
                    (getTreesInLineOfSight forest x y maxX maxY
                     |> Seq.map getTallest
                     |> Seq.min) < tree
    }
    |> Seq.filter id
    |> Seq.length
    |> int64


let part2 fn () =

    let forest, maxX, maxY = makeForest fn

    seq {
        for y in [ 1 .. maxX - 2 ] do
            for x in [ 1 .. maxY - 2 ] do
                let tree = forest.[y][x]

                yield
                    getTreesInLineOfSight forest x y maxX maxY
                    |> Seq.map (Seq.takeWhilePlus1 (fun h -> h < tree) >> Seq.length >> int64)
                    |> Seq.reduce (*)
    }
    |> Seq.max
