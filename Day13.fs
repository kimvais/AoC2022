module AoC2022.Day13

open System
open AoC2022.Utils
open FSharp.Data

open System.Runtime.CompilerServices

type Provider = JsonProvider<"input/test13.txt", SampleIsList=true>

type NestedList =
    | Integer of int
    | List of NestedList list

// Define an active pattern for parsing integers
let (|Integer|_|) (input: string) =
    match System.Int32.TryParse(input) with
    | true, i -> Some (Integer i)
    | _ -> None

// Define an active pattern for parsing nested lists
let rec (|List|_|) (input: string) =
    if input.StartsWith("[") && input.EndsWith("]") then
        let listString = input.Substring(1, input.Length - 2)
        let elements = parseList listString
        Some (List elements)
    else None

and parseList (input: string) : NestedList list =
    // Split the input string into individual elements, using ',' as the delimiter
    let elements = input.Split(',', StringSplitOptions.RemoveEmptyEntries)

    // Use pattern matching to process each element
    let rec parseElement (e: string) : NestedList =
        printfn $"%A{e}"
        // Use the active patterns to parse and match the element
        match e with
        | Integer i -> Integer i
        | List l -> List l
        | _ -> failwith "Invalid input"

    // Map the parsed elements to a list of NestedList values
    elements |> Array.map parseElement |> List.ofArray 


let parseRecord r =
    let pair = r |> splitByLinefeed
    let left = parseList (Seq.head pair)
    let right = parseList (Seq.last pair)
    printfn $"%A{left} %A{right}"
    left, right
    
let part1 fn () =
    let records = readInputDelimByEmptyLine fn |> Seq.map parseRecord
    printfn $"%A{records}"
    // records |> Seq.map (parseRecord) |> printfn "%A"
    0L

let part2 fn () = 0L
