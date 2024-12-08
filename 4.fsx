open System
open System.Text.RegularExpressions

let input =
    System.IO.File.ReadAllLines "4.txt"
    |> Array.map (fun x -> x.ToCharArray() |> Array.map string |> Array.toList)
    |> Array.toList




let parse item acc count =
    match item, acc with
    | "X", _ -> "X", count
    | "M", "X" -> "XM", count
    | "A", "XM" -> "XMA", count
    | "S", "XMA" -> "", count + 1
    | _, _ -> "", count






type Direction =
    | DownRight
    | DownLeft

let tryGet i1 i2 =
    try
        Some input.[i1].[i2]
    with _ ->
        None



let rec go dir i1 i2 acc =
    let item = tryGet i1 i2

    match item with
    | Some item ->
        let i2 =
            (match dir with
             | DownRight -> i2 + 1
             | DownLeft -> i2 - 1)

        go dir (i1 + 1) i2 (acc @ [ item ])
    | None -> acc

let traverse dir i1 i2 = go dir i1 i2 []


let countRow row =
    let result =
        (row
         |> List.fold (fun (word, count) item -> parse item word count) ("", 0)
         |> snd)
        + (row
           |> List.rev
           |> List.fold (fun (word, count) item -> parse item word count) ("", 0)
           |> snd)

    result

let count = List.map countRow >> List.sum



let horizontal = input

let vertical =
    input
    |> List.mapi (fun i1 values -> values |> List.mapi (fun i2 _ -> input.[i2].[i1]))

let diagonal =
    input
    |> List.mapi (fun i1 values ->
        values
        |> List.mapi (fun i2 _ ->
            let iMax = input.Length - 1

            match i1, i2 with
            | 0, i2 -> (traverse DownRight i1 i2) @ (traverse DownLeft i1 i2)
            | i1, 0 -> (traverse DownRight i1 i2)
            | i1, i2 when i2 = iMax -> (traverse DownLeft i1 i2)
            | _, _ -> []

        )
        |> List.collect id)




// Part 1
printf "%i " (count diagonal + count vertical + count horizontal)
