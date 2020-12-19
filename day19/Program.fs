// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open day19.Types
open day19.Parser 
open day19.IO

let rec matches (rule:Rule) (chars:List<char>) (exhaustive:bool): bool*List<Char> =
    printfn "matches %s %A %A" (toS rule) chars exhaustive
    let checkExhaustive (res:bool*List<char>) =
        match exhaustive,res with
        | false,_ -> res
        | true,(false,_) -> res
        | true,(true,[]) -> res
        | true,(true,_) -> false,chars                 
    match rule, chars with
    | A, 'a':: tail -> checkExhaustive(true,tail)
    | A, _  -> false, chars 
    | B, 'b':: tail -> checkExhaustive(true,tail)
    | B, _ -> false, chars 
    | Or ([||]), list -> false,chars
    | Or (subs), list ->
        let subMatch = checkExhaustive(matches subs.[0] chars exhaustive)
        match subMatch with
        | false,_ -> matches (Or(subs.[1..(subs.Length-1)])) chars exhaustive  
        | true,_ -> subMatch
    | Rules ([||]), list -> checkExhaustive(true,list)
    | Rules (subs), list ->
        let subMatch = matches (subs.[0]) chars false 
        match subMatch with
        | false,_ -> false,chars
        | true,tail -> matches (Rules(subs.[1..(subs.Length-1)])) tail exhaustive
//    | Ref _, list -> 
        
           
    
    
let checkMatch (rule:Rule) (message:String) : bool =
    let chars = message.ToCharArray () |> List.ofArray
    let matched = matches rule chars true 
    let res = fst matched
    let rest = snd matched 
    printfn "Match result: %A with rest %A" res rest
    res

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
//    printfn "Compressed rules: %A" compressedPrint  
    let map = toMap compressed
    let expMap = expandMap map
    printfn "Expanded rules:"
    expMap |> Map.toArray |> Array.map (fun ir -> printfn "%A: %A" (fst ir) (toS (snd ir)))
    let rule = expMap.[0]
    let _ = checkMatch rule "ababbb" 
//    let matched = data |> Array.filter (checkMatch rule)
//    printfn "%A" matched  
    0
    
    
    