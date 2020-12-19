// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open day19.Types
open day19.Parser 
open day19.IO

let rec matches (rule:Rule) (chars:List<char>) (exhaustive:bool) (rules:RuleMap): bool*List<Char> =
//    printfn "matches %s %A %A" (toS rule) (chars |> String.Concat) exhaustive
    let checkExhaustive (res:bool*List<char>) =
        let r = match exhaustive,res with
                | false,_ -> res
                | true,(false,_) -> res
                | true,(true,[]) -> res
                | true,(true,_) -> false,chars
        // printfn "<== (%A %s) : %A %s %A" (fst r) (snd r |> String.Concat) (toS rule) (chars |> String.Concat) exhaustive   
        r 
    match rule, chars with
    | A, 'a':: tail -> checkExhaustive(true,tail)
    | A, _  -> false, chars 
    | B, 'b':: tail -> checkExhaustive(true,tail)
    | B, _ -> false, chars 
    | Or ([]), _ -> false,chars
    | Or (sub::subs), _ ->
        let subMatch = checkExhaustive(matches sub chars exhaustive rules) 
        match subMatch with
        | false,_ -> matches (Or(subs)) chars exhaustive rules  
        | true,_ -> subMatch
    | Rules ([]), list -> checkExhaustive(true,list)
    | Rules (sub::subs), _ ->
        let subMatch = matches sub chars false rules
        match subMatch with
        | false,_ -> false,chars
        | true,tail -> matches (Rules subs) tail exhaustive rules 
    | Ref ref, _ -> matches rules.[ref] chars exhaustive rules 
        
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
    let matched = matches rule chars true rules  
    let res = fst matched 
    let rest = snd matched 
//    printfn "Match result: %A with rest %A" res rest
    res

[<EntryPoint>]
let main argv =
    let input = readFile "/Users/xeno/projects/aoc2020/day19_fs/input.txt" |> Seq.toArray
    let rulesAndData = splitBySpace input
    let rules = fst rulesAndData
    let data = snd rulesAndData 
    printfn "%A" rulesAndData
    let parsed = rules |> Array.map parseRule
    let parsedPrint = parsed |> Array.map (fun s -> (fst s, toS (snd s)))   
//    printfn "Parsed rules: %A" parsedPrint
    let compressed = parsed |> Array.map (fun s -> (fst s, compress (snd s)))
    let compressedPrint = compressed |> Array.map (fun s -> (fst s, toS (snd s)))
//    printfn "Compressed rules: %A" compressedPrint  
    let map = toMap compressed
    let rule = map.[0]
    let matched = data |> Array.filter (checkMatch map map.[0])
    printfn "%A %A" matched matched.Length  
//    let expMap = expandMap map
//    printfn "Expanded rules:"
//    expMap |> Map.toArray |> Array.map (fun ir -> printfn "%A: %A" (fst ir) (toS (snd ir)))
//    let _ = checkMatch rule "ababbb"
//    let map2 = updateMap map
//    printfn "Updated rules:"
//    map2 |> Map.toArray |> Array.map (fun ir -> printfn "%A: %A" (fst ir) (toS (snd ir)))
  
    let s1 = "baba"  
    let s1 = "aaaaabbaabaaaaababaa"
//    printfn "Res: %A" (checkMatch map2 map2.[0] s1) 
    let map2 = map
    0
    
    
    