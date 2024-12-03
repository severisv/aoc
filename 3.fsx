open System
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines "3.txt" |> String.concat "\n"

let mul (m: string) =
    m.Substring(4, m.Length - 5).Trim().Split(',')
    |> Array.map int
    |> (fun [| a; b |] -> a * b)

// Part 1
Regex.Matches(input, @"mul\(\d+,\d+\)")
|> Seq.toList
|> List.map (fun m -> m.Value)
|> List.map (mul)
|> List.sum
|> printf "%i\n"


// Part 2
let instructions =
    Regex.Matches(input, @"mul\(\d+,\d+\)|do\(\)|don't\(\)")
    |> Seq.toList
    |> List.map (fun m -> m.Value)

instructions
|> List.fold
    (fun (shouldDo, acc) item ->
        match (item, shouldDo) with
        | "do()", _ -> true, acc
        | "don't()", _ -> false, acc
        | _, true -> shouldDo, acc + mul item
        | _, false -> shouldDo, acc)
    (true, 0)
|> snd
|> printf "%i\n"
