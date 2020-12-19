// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open day19.Types
open day19.Parser 
open day19.IO

let rec matches (pattern:list<Rule>) (chars:List<char>) (lookup:(int->Rule)) : bool*List<Char> =
//    printfn "matches %A %A" pattern (chars |> String.Concat)
    match pattern, chars with
    | [],[] -> true,[]
    | [], _::tail -> false, tail  
    | A::rules, 'a':: tail -> matches rules tail lookup 
    | A::_, _  -> false, chars 
    | B::rules, 'b':: tail -> matches rules tail lookup 
    | B::_, _ -> false, chars 
    | (Or [])::_, _ -> false,chars
    | (Or (sub::subs))::rules, _ ->
        let subMatch = matches (sub::rules) chars lookup 
        match subMatch with
        | false,_ -> matches ((Or subs)::rules) chars lookup 
        | true,_ -> subMatch
    | (Rules [])::rules, list -> matches rules chars lookup 
    | (Rules (Or(alts)::tail))::rules, _ -> // todo
        let newAlts = alts |> List.map (fun alt -> Rules(alt::tail))
        let rewrite = Or(newAlts)::rules                                          
        in matches rewrite chars lookup 
    | (Rules (Ref(i)::tail))::rules,_ ->
        let newRules = Rules((lookup i)::tail)
        let rewrite = newRules::rules 
        in matches rewrite chars lookup 
    | (Rules (sub::subs)::rules), _ ->
        let remaining = Rules(subs)::rules 
        matches (sub::remaining) chars lookup 
    | (Ref ref)::rules, _ -> matches ((lookup ref)::rules) chars lookup  
        
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
    let lookup : (int -> Rule) = (fun i -> rules.[i])
    let matched = matches [rule] chars lookup 
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
    let input = readRules "/Users/xeno/projects/aoc2020/day19_fs/input.txt"
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
    
    
    