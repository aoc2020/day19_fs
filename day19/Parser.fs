module day19.Parser

open System
open day19.Types

let parseItem (s:String) : Rule =
    let st = s.Trim ()
    printfn "parseItem[%s]" s
    if s = "\"a\"" then A
    else if s = "\"b\"" then B
    else Ref (st |> int)

let parseSubRule (s:String) : Rule  =
    let ts = s.Trim() 
//    printfn "parseSubRule (%A)" ts 
    let rules = ts.Split ' ' |> Array.map parseItem |> List.ofSeq 
    Rules rules
    
let rec compress (r:Rule) : Rule = 
    match r with
    | (Rules r) when r.Length = 1 -> compress r.[0]
    | (Rules rs) -> Rules (rs |> List.map compress) 
    | (Or r) when r.Length = 1 -> compress r.[0]
    | (Or rs) -> Or (rs |> List.map compress)
    | _ -> r 

let parseRule (s:String) : int*Rule =
    let split = s.Split ":"
    let index = split.[0] |> int
    let subs = split.[1].Split "|"
    let subRules = subs |> Array.map parseSubRule |> List.ofSeq     
    printfn "%A ==> ((%A))" index subRules
    index, (Or subRules) 

type RuleMap = Map<int,Rule>

let toMap (rules:(int*Rule)[]) : RuleMap =
    rules |> Map.ofSeq     

let rec expand (rule:Rule) (rules:RuleMap) : Rule =
    let exp r = expand r rules  
    match rule with
    | Ref index -> exp rules.[index] 
    | Rules rules -> Rules (rules |> List.map exp) 
    | Or rules -> Or (rules |> List.map exp)
    | A -> A
    | B -> B
    
let expandMap (rules:RuleMap) : RuleMap =
    let exp (ir:int*Rule) : int*Rule = (fst ir, expand (snd ir) rules)
    rules |> Map.toSeq |> Seq.map exp |> Map.ofSeq 