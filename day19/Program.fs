﻿// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open day19.Types
open day19.Parser 
open day19.IO

let rec matches (rules:list<Rule>) (chars:List<char>) (exhaustive:bool) (lookup:(int->Rule)) : bool*List<Char> =
//    printfn "matches %s %A %A" (toS rule) (chars |> String.Concat) exhaustive
    let checkExhaustive (res:bool*List<char>) =
        let r = match exhaustive,res with
                | false,_ -> res
                | true,(false,_) -> res
                | true,(true,[]) -> res
                | true,(true,_) -> false,chars
        // printfn "<== (%A %s) : %A %s %A" (fst r) (snd r |> String.Concat) (toS rule) (chars |> String.Concat) exhaustive   
        r 
    match rules, chars with
    | [A], 'a':: tail -> checkExhaustive(true,tail)
    | [A], _  -> false, chars 
    | [B], 'b':: tail -> checkExhaustive(true,tail)
    | [B], _ -> false, chars 
    | [Or ([])], _ -> false,chars
    | [Or (sub::subs)], _ ->
        let subMatch = checkExhaustive(matches [sub] chars exhaustive lookup) 
        match subMatch with
        | false,_ -> matches [Or subs] chars exhaustive lookup 
        | true,_ -> subMatch
    | [Rules []], list -> checkExhaustive(true,list)
    | [Rules (Or(ors)::tail)], _ ->
        let rewrite = [Or(ors |> List.map (fun o -> Rules (o::tail)))]
        in matches rewrite chars exhaustive lookup 
    | [Rules (Ref(i)::tail)],_ ->
        let rewrite = [Rules((lookup i)::tail)]
        in matches rewrite chars exhaustive lookup 
    | [Rules (sub::subs)], _ ->
        let subMatch = matches [sub] chars false lookup 
        match subMatch with
        | false,_ -> false,chars
        | true,tail -> matches ([Rules subs]) tail exhaustive lookup 
    | [Ref ref], _ -> matches ([lookup ref]) chars exhaustive lookup  
        
let updateMap (map:RuleMap) : RuleMap =
    let _42 = Ref 42
    let _8 = Ref 8
    let _11 = Ref 11
    let _31 = Ref 31
    let new8 : Rule = Or [_42;Rules [_42;_8]]
    let new11 : Rule = Or [Rules [_42;_31];Rules [_42;_11;_31]]
    let m1 = map.Add (8,new8)
    let m2 = m1.Add (11,new11)
    m2 
 
let checkMatch (rules:RuleMap) (rule:Rule) (message:String): bool =
    let chars = message.ToCharArray () |> List.ofArray
    let lookup = (fun i -> rules.[i])
    let matched = matches [rule] chars true lookup 
    let res = fst matched 
    let rest = snd matched 
//    printfn "Match result: %A with rest %A" res rest
    res

type InputData (rules:RuleMap, messages:String[]) as self =
    member this.Rules = rules 
    member this.Messages = messages  

let readRules (input:String) : InputData  =
    let input = readFile input |> Seq.toArray
    let rulesAndData = splitBySpace input
    let rules = fst rulesAndData
    let data = snd rulesAndData 
    let parsed = rules |> Array.map parseRule
    let compressed = parsed |> Array.map (fun s -> (fst s, compress (snd s)))
    let map = toMap compressed
    InputData (map,data) 

let modify (input: InputData) =
    let newRules = updateMap input.Rules
    in InputData(newRules,input.Messages) 

[<EntryPoint>]
let main argv =
    let task = 2
    let input = readRules "/Users/xeno/projects/aoc2020/day19_fs/input3.txt"
    printfn "%A" input
    let active =
        if task = 1 then
            input
        else
            modify input
    let rule = active.Rules.[0]
    let matched = input.Messages |> Array.filter (checkMatch active.Rules rule)
    printfn "%A %A" matched matched.Length  

//    let s1 = "baba"  
//    let s1 = "aaaaabbaabaaaaababaa"
//    printfn "Res: %A" (checkMatch map2 map2.[0] s1) 
//    let map2 = map
    0
    
    
    