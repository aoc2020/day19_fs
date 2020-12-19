module day19.Types

open System

type Rule =
    | A
    | B 
    | Ref of int
    | Rules of Rule[]
    | Or of Rule[] 


let rec toS (r:Rule) : String =
    match r with
    | A -> "A"
    | B -> "B"
    | Ref x -> sprintf "%d" x
    | Or rs -> sprintf "(%s)" (rs |> Seq.map toS |> String.concat "|")
    | Rules rs -> sprintf "%s" (rs |> Seq.map toS |> String.concat "")



