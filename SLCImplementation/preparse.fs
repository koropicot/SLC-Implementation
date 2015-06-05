[<FunScript.JS>]
module preparse
open lexer
open parser
open form

type token = 
    | LEFT | RIGHT | REC | EQ | DEF
    | PLUS | MINUS | TIMES
    | HAT | QMARK
    | PA | REN | BRA | CKET | COMMA
    | IDENT of string
    | NUM of int
    | EOF

[<FunScript.JSEmit("return Number({0})")>]
let number x = failwith "never"

let patterns = [|
        (@"\s", skip);
#if TOJS
        (@"[0-9]+", value >> number >> NUM >> Some);
#else
        (@"[0-9]+", value >> int >> NUM >> Some);
#endif
        (":=", constant DEF);
        ("<=", constant LEFT); ("=>", constant RIGHT); ("rec", constant REC); ("=", constant EQ);
        ("\+", constant PLUS); ("\-", constant MINUS); ("\*", constant TIMES);
        ("\^", constant HAT); ("\?", constant QMARK);
        ("\(", constant PA); ("\)", constant REN); ("{", constant BRA); ("}", constant CKET); (",", constant COMMA);
        (@"[a-zA-Z_'][a-zA-Z0-9_']*", value >> IDENT >> Some)
    |]

let curry f a b = f (a,b)

let ident s = terminate (function | IDENT name -> Some name | _ -> None) <| s
let num s = terminate (function | NUM num -> Some num | _ -> None) <| s

let rec form s =
    eform
    <| s
and eform s =
    pform %& opt (terminal EQ %&. pform) 
        +>= fun (l, o) -> match o with | Some r -> SEql(l, r) | None -> l
    <| s
and pform s =
    chainl tform
        (terminal PLUS +> curry SAdd .| terminal MINUS +> curry SSub)
    <| s
and tform s =
    chainl hform (terminal TIMES +> curry SMul)
    <| s
and hform s =
    chainl qform (terminal HAT +> curry SUp)
    <| s
and qform s =
    chainr nform (terminal QMARK +> curry SDwn)
    <| s
and nform s =
    num +>= SNum
    .| terminal REC %&. sform .%& terminal EQ %& form +>= SRec
    .| sform %& opt ((terminal LEFT %&. form) %| (terminal RIGHT %&. form)) 
        +>= fun (h, o) ->
            match o with
            | Some(Left body) -> SLft(h, body)
            | Some(Right body) -> SRgt(h, body)
            | None -> h
    <| s
and sform s =
    terminal PA %&. forms .%& terminal REN +>= SPar
    .| terminal BRA %&. forms .%& terminal CKET +>= SBrc
    .| ident +>= SIde
    <| s
and forms s =
    flist
    .| ret []
    <| s
and flist s = 
    form %& rep (terminal COMMA %&. form) +>= fun (h, t) -> h :: t
    <| s

let start s =
    ident .%& terminal DEF %& form  .%& terminal EOF +>= Def
    .| form .%& terminal EOF +>= Eval
    <| s

let pre_parse s =
    let tokens = tokenize EOF patterns s
    parse start tokens
