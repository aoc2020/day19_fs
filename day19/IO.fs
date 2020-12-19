module day19.IO

open System
open System.IO


let readFile (filePath:String) = seq {
    use sr = new StreamReader ( filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let splitBySpace (input:String[]) =
    let blankLine = input |> Seq.findIndex (fun (s:String) -> s = "")
    let rules = input.[0..blankLine-1]
    let data = input.[blankLine+1..input.Length-1]
    rules, data      
