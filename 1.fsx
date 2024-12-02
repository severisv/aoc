open System

let list1 = [ 1 ]
let list2 = [ 2 ]


list1
|> List.sort
|> List.zip (list2 |> List.sort)
|> List.map (fun (a, b) -> a - b |> Math.Abs)
|> List.sum
|> printf "%i"



list1
|> List.map (fun v ->
    list2
    |> List.groupBy id
    |> List.tryFind (fun (key, _) -> key = v)
    |> Option.map (fun (_, values) -> (values |> List.length) * v)
    |> Option.defaultValue 0)
|> List.sum
|> printfn "%i"
