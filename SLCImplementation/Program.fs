module Program
open System
open form
open Lexer
open Parser 
open Microsoft.FSharp.Text.Lexing

//string -> tokens -> form
let parse = LexBuffer<_>.FromString >> Parser.start Lexer.token

[<EntryPoint>]
let main args =
    Console.ReadLine() |> parse |> Console.WriteLine 
    0
