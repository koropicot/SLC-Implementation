﻿module Program
open System
open form
open preparse
open toplev

//REPL
let rec repl () =
    printf "> "
    let line =
      Console.ReadLine()
      |> function null -> ":quit" | l -> l.Trim()
    match line with
    | ":quit" -> printfn "Exit..."
    | ":help" | "" ->
        printfn "Usage:"
        printfn "> expression"
        printfn "    evaluate expression"
        printfn "> name := expression"
        printfn "    evaluate expression and define it"
        printfn "> :quit"
        printfn "    quit repl"
        printfn "> :help"
        printfn "    show this usage"
        repl ()
    | s ->
        try
            match pre_parse s with
            | Def (n, e) -> printfn "%s" (zd_to_str n e)
            | Eval e -> printfn "%s" (z_to_str e)
        with ex ->
            printfn "Error: %s" ex.Message
            printfn ":help for usage"
        repl ()

[<EntryPoint>]
let main args =
    repl ()
    0
