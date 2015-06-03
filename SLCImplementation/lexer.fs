module lexer
open System.Text.RegularExpressions

let constant x _ = Some x
let skip _ = None
let value (m: Match) = m.Value

let tokenize eof patterns source =
    let regexPatterns = patterns |> Array.map (fun (pat, f) -> (new Regex(pat, RegexOptions.Compiled), f)) 
    let rec tokenize_ (raw: string) pos =
        if pos >= raw.Length then [eof] else
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
    tokenize_ source 0 |> Array.ofList
