module preparse
open lexer
open parser
open form

type token = 
    | LEFT | RIGHT | REC | EQ
    | PLUS | MINUS | TIMES
    | HAT | QMARK
    | PA | REN | BRA | CKET | COMMA
    | IDENT of string
    | NUM of int
    | EOF

let patterns = [|
        (@"\s", skip); (@"[0-9]+", value >> int>> NUM >> Some);
        ("<=", constant LEFT); ("=>", constant RIGHT); ("rec", constant REC); ("=", constant EQ);
        ("\+", constant PLUS); ("\-", constant MINUS); ("\*", constant TIMES);
        ("\^", constant HAT); ("\?", constant QMARK);
        ("\(", constant PA); ("\)", constant REN); ("{", constant BRA); ("}", constant CKET); (",", constant COMMA);
        (@"[a-zA-Z_'][a-zA-Z0-9_']*", value >> IDENT >> Some)
    |]

let rec start (s: token[] * int) =
    form .%& terminal EOF
    <| s
and form s =
    eform
    <| s
and eform s =
    pform %& opt (terminal EQ %&. pform) 
        +>= fun (l, o) -> match o with | Some r -> SEql(l, r) | None -> l
    <| s
and pform s =
    chainl tform
        (terminal PLUS +> (fun a b -> SAdd(a, b)) .| terminal MINUS +> (fun a b -> SSub(a, b)))
    <| s
and tform s =
    chainl hform (terminal TIMES +> fun a b -> SMul(a, b))
    <| s
and hform s =
    chainl qform (terminal HAT +> fun a b -> SUp(a, b))
    <| s
and qform s =
    chainr nform (terminal QMARK +> fun a b -> SDwn(a, b))
    <| s
and nform s =
    num +>= SNum
    .| terminal REC **+ sform **+ terminal EQ **. form +>= fun r -> SRec (_2 r, _4 r)
    .| sform %& opt ((terminal LEFT %&. form) %| (terminal RIGHT %&. form)) 
        +>= fun (h, o) ->
            match o with
            | Some (Left body) -> SLft (h, body)
            | Some (Right body) -> SRgt (h, body)
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
and ident s = terminate (function | IDENT name -> Some name | _ -> None) <| s
and num s = terminate (function | NUM num -> Some num | _ -> None) <| s

let pre_parse = tokenize EOF patterns >> parse start
