module day19.Parser

open System
open day19.Types

let parseItem (s:String) : Rule =
//    printfn "parseItem[%A]" s
    if s = "\"a\"" then A
    else if s = "\"b\"" then B
    else Ref (s |> int)

let parseSubRule (s:String) : Rule  =
    let ts = s.Trim() 
//    printfn "parseSubRule (%A)" ts 
    let rules = ts.Split ' ' |> Array.map parseItem
    Rules rules
    
let rec compress (r:Rule) : Rule = 
    match r with
    | (Rules r) when r.Length = 1 -> compress r.[0]
    | (Rules rs) -> Rules (rs |> Array.map compress) 
    | (Or r) when r.Length = 1 -> compress r.[0]
    | (Or rs) -> Or (rs |> Array.map compress)
    | _ -> r 

let parseRule (s:String) : int*Rule =
    let split = s.Split ":"
    let index = split.[0] |> int
    let subs = split.[1].Split "|"
    let subRules = subs |> Array.map parseSubRule    
    printfn "%A ==> ((%A))" index subRules
    index, (Or subRules) 

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
