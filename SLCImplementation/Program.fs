module Program
open System
open form
open Lexer
open Parser 
open Microsoft.FSharp.Text.Lexing
open toplev

//string -> tokens -> form
let pre_parse = LexBuffer<_>.FromString >> Parser.start Lexer.token

//REPL
let rec repl () =
    printf ">"
    match Console.ReadLine().Trim().Split(' ', '\t') |> List.ofArray |> List.filter ((<>) "") with
    | ["quit"] -> ()
    | ["help"] | ["h"] ->
        printfn "Usage:"
        printfn "    >expression"
        printfn "        evaluate expression"
        printfn "    >def name expression"
        printfn "        evaluate expression and define it"
        printfn "    >quit"
        printfn "        quit repl"
        repl ()
    | "def"::x::((_::_) as s)->
        try zd x (pre_parse (List.reduce (+) s)) with ex -> printfn "Error: %s" ex.Message
        repl ()
    | _::_ as s ->
        try z (pre_parse (List.reduce (+) s)) with ex -> printfn "Error: %s" ex.Message
        repl ()
    | _ ->
        printfn "show usage: >help"
        repl ()

[<EntryPoint>]
let main args =
    repl ()
    0
