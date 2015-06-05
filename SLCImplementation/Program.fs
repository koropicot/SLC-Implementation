[<FunScript.JS>]
module Program
open System
open form
open preparse
open toplev
open FunScript
open FunScript.TypeScript

//REPL
let rec repl () =
    printf ">"
    match Console.ReadLine().Trim() with
    | "quit" -> ()
    | "help" | "" ->
        printfn "Usage:"
        printfn "    >expression"
        printfn "        evaluate expression"
        printfn "    >name := expression"
        printfn "        evaluate expression and define it"
        printfn "    >quit"
        printfn "        quit repl"
        repl ()
    | s ->
        try
            match pre_parse s with
            | Def (n, e) -> printfn "%s" (zd_to_str n e)
            | Eval e -> printfn "%s" (z_to_str e)
        with ex ->
            printfn "Error: %s" ex.Message
            printfn "show usage: >help"
        repl ()




//JSFFI
[<JSEmit("return createOutputCard({0}, {1}, {2}, {3})")>]
let createOutputCard (code: string) (output: string) (error: bool) (name: string): 'TAny = failwith "never"

let submit s = 
    try
        match pre_parse s with
        | Def (n, e) -> createOutputCard s (zd_to_str n e) false n
        | Eval e -> createOutputCard s (z_to_str e) false ""
    with ex ->
        createOutputCard s ("Error: " + ex.Message) true ""

#if TOJS
[<EntryPoint>]
let main args =
    let sw = new IO.StreamWriter("slc.js")
    Compiler.compile(<@ submit @>) |> sprintf "function getSubmit(){\n%s;\n}" |> sw.Write
    sw.Close()
    0
#else
[<EntryPoint>]
let main args =
    repl ()
    0
#endif


