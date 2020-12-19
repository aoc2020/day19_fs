// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open day19.Types
open day19.Parser 
open day19.IO

type RuleMap = Map<int,Rule>

let toMap (rules:(int*Rule)[]) : RuleMap =
    rules |> Map.ofSeq     

let rec expand (rule:Rule) (rules:RuleMap) : Rule =
    let exp r = expand r rules  
    match rule with
    | Ref index -> exp rules.[index] 
    | Rules rules -> Rules (rules |> Array.map exp) 
    | Or rules -> Or (rules |> Array.map exp)
    | A -> A
    | B -> B
    
let expandMap (rules:RuleMap) : RuleMap =
    let exp (ir:int*Rule) : int*Rule = (fst ir, expand (snd ir) rules)
    rules |> Map.toSeq |> Seq.map exp |> Map.ofSeq 

[<EntryPoint>]
let main argv =
    let input = readFile "/Users/xeno/projects/aoc2020/day19_fs/input2.txt" |> Seq.toArray
    let rulesAndData = splitBySpace input
    let rules = fst rulesAndData
    let data = snd rulesAndData 
    printfn "%A" rulesAndData
    let parsed = rules |> Array.map parseRule
    let parsedPrint = parsed |> Array.map (fun s -> (fst s, toS (snd s)))   
//    printfn "Parsed rules: %A" parsedPrint  
    let compressed = parsed |> Array.map (fun s -> (fst s, compress (snd s)))
    let compressedPrint = compressed |> Array.map (fun s -> (fst s, toS (snd s)))
    printfn "Compressed rules: %A" compressedPrint  
    let map = toMap compressed
    let expMap = expandMap map
    expMap |> Map.toArray |> Array.map (fun ir -> printfn "%A: %A" (fst ir) (toS (snd ir)))
    0
    