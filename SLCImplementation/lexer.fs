module lexer
open System.Text.RegularExpressions

type token = 
    | LEFT | RIGHT | REC | EQ
    | PLUS | MINUS | TIMES
    | HAT | QMARK
    | PA | REN | BRA | CKET | COMMA
    | IDENT of string
    | NUM of int
    | EOF

let constant x _ = Some x
let skip _ = None
let value (m: Match) = m.Value

let patterns = [|
        (@"\s", skip); (@"[0-9]+", value >> int>> NUM >> Some);
        ("<=", constant LEFT); ("=>", constant RIGHT); ("rec", constant REC); ("=", constant EQ);
        ("\+", constant PLUS); ("\-", constant MINUS); ("\*", constant TIMES);
        ("\^", constant HAT); ("\?", constant QMARK);
        ("\(", constant PA); ("\)", constant REN); ("{", constant BRA); ("}", constant CKET); (",", constant COMMA);
        (@"[a-zA-Z_'][a-zA-Z0-9_']*", value >> IDENT >> Some)
    |]

let regexPatterns = patterns |> Array.map (fun (pat, f) -> (new Regex(pat, RegexOptions.Compiled), f)) 

let rec tokenize_ (raw: string) pos =
    if pos >= raw.Length then [EOF] else
        let ret = regexPatterns
                    |> Array.tryPick (fun (reg, f) ->
                        let m = reg.Match(raw, pos)
                        if m.Success && m.Index = pos then Some (f m, pos + m.Length) else None)
        match ret with
            | Some (res, next) ->
                match res with
                | Some token -> token :: tokenize_ raw next
                | None -> tokenize_ raw next
            | None -> failwith (sprintf "SyntaxError: Unexpected char: '%s' Pos: %d" (raw.Substring(pos, 1)) pos)

let tokenize source = tokenize_ source 0 |> Array.ofList
