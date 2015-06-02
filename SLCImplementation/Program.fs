module Program
open System
open form
open preparse
open toplev

//REPL
let rec repl () =
    printf ">"
    match Console.ReadLine().Trim() with
    | "quit" -> ()
    | "help" | "" ->
        printfn "Usage:"
        printfn "    >expression"
        printfn "        evaluate expression"
        printfn "    >def name expression"
        printfn "        evaluate expression and define it"
        printfn "    >quit"
        printfn "        quit repl"
        repl ()
    | s ->
        try
            match pre_parse s with
            | Def (n, e) -> zd n e
            | Eval e -> z e
        with ex ->
            printfn "Error: %s" ex.Message
            printfn "show usage: >help"
        repl ()

[<EntryPoint>]
let main args =
    repl ()
    0
