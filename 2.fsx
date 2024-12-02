open System

let input = 
    System.IO.File.ReadAllLines "2.txt"
    |> Array.map (fun x -> x.Split(' ') |> Array.map int |> Array.toList)
    |> Array.toList

type Direction = Asc | Desc

let getErrorCount prev current dir = 
    match dir with
    | Asc when [1; 2; 3] |> List.contains (current - prev) -> 0
    | Desc when [-1; -2; -3] |> List.contains (current - prev) -> 0
    | _ -> 1

let trd (_, _, x) = x

let countErrors list =
    list
    |> List.fold (fun (prevItem, dir, errorCount) item -> 
        let (dir, newErrorCount) = 
            match (prevItem, dir) with
            | (None, None) -> None, 0
            | (Some prev, None) -> 
                let dir = if item - prev > 0 then Asc else Desc
                Some dir, getErrorCount prev item dir
            | (Some prev, Some dir) ->
                Some dir, errorCount + getErrorCount prev item dir

        (Some item, dir, newErrorCount)
    ) (None, None, 0)
    |> trd

// Part 1
input 
|> List.filter (fun x -> (x |> countErrors) = 0)
|> List.length
|> printf "%i\n"

// Part 2
input 
|> List.map (fun list -> 
    let permutations =
        list 
        |> List.mapi (fun i _ -> 
            list 
            |> List.mapi (fun i2 item -> if i = i2 then None else Some item)
            |> List.choose id
        )                
    permutations 
    |> List.map countErrors
    |> List.min
)
|> List.filter (fun x -> x = 0)
|> List.length
|> printf "%i\n"
