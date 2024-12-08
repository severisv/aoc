open System
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines "4.txt" 
            |> Array.map (fun x -> x.ToCharArray() |> Array.map string |> Array.toList) 
            |> Array.toList




let parse item word count =
    match item, word with
    | "S", "" -> "S", count
    | "A", "S" -> "SA", count
    | "M", "SA" -> "SAM", count
    | "X", "SAM" -> "SAMX", count + 1
    | _ -> "", count






type Direction = Right | Left
let tryGet dir i i1 i2 = 
    try 
        Some input.[i1+i].[i2 + (match dir with | Right -> i | Left -> -i)]
    with | _ -> None

let traverse dir iMax i1 i2 = [0 .. iMax] 
                             |> List.map (tryGet dir i1 i2) |> List.choose id


let countRow row = 
        let result =
                    (row 
                    |> List.fold (fun (word, count) item -> 
                            parse item word count) ("", 0) |> snd)
                    + (row |> List.rev |> List.fold (fun (word, count) item -> 
                            parse item word count) ("", 0) |> snd)
        result
    
let count input  = 
    input |> List.map (countRow)
    |> List.sum



let horizontal = input 

let vertical = 
    input 
    |> List.mapi (fun i1 values  -> 
                        values |> List.mapi (fun i2 _ ->  input.[i2].[i1]
                        ))

let diagonal =
    input 
    |> List.mapi (fun i1 values  -> 
                        values |> List.mapi (fun i2 _ ->  
                            let iMax = input.Length - 1
                            let traverseLeft = traverse Left iMax
                            let traverseRight = traverse Right iMax
                            match i1, i2 with
                            | 0, i2 -> (traverseRight i1 i2) @ (traverseLeft i1 i2)
                            | i1, 0 ->
                                   (traverseRight i1 i2)
                            | i1, i2 when i2 = iMax ->
                                   (traverseLeft i1 i2)
                            | _, _ -> []
                            
                        ))
                        |> List.collect id




printf "%i "(count diagonal + count vertical + count horizontal)


