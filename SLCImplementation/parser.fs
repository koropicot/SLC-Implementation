module parser
open lexer
open form

type Parser<'s, 'r> = 's[] * int->Option<'r * int>
type Either<'l, 'r> = | Left of 'l | Right of 'r

let terminate (p:'s->Option<'r>): Parser<'s, 'r>= fun (s, i) -> match p s.[i] with | Some r -> Some (r, i+1) | None -> None
let terminal t = terminate (fun s -> if s = t then Some t else None)

//monadic
let ret r: Parser<'s, 'r> = fun (s, i) -> Some (r, i)
let (>>=) (p:Parser<'s, 'a>) (f:'a->Parser<'s, 'b>) = fun (s, i) -> p (s, i) |> Option.bind (fun (r, ri) -> (f r) (s, ri))
let (+>=) p f = p >>= (f >> ret)
let (+>) p r = p >>= fun _ -> ret r

//and
let (%&) (pa:Parser<'s, 'a>) (pb:Parser<'s, 'b>) = pa >>= fun a -> pb >>= fun b -> ret (a, b)
let (.%&) (pa:Parser<'s, 'a>) (pb:Parser<'s, 'b>) = pa %& pb +>= fst
let (%&.) (pa:Parser<'s, 'a>) (pb:Parser<'s, 'b>) = pa %& pb +>= snd

//or
let (%|) (pa:Parser<'s, 'a>) (pb:Parser<'s, 'b>) =
    fun (s, i) -> 
        match pa (s, i) with
        | Some (r, ri) -> Some (Left r, ri)
        | None ->
            match pb (s, i) with
            | Some (r, ri) -> Some (Right r, ri)
            | None -> None 
let (.|) pa pb = pa %| pb +>= function | Left l -> l | Right r -> r

//chain
let rec rep (p:Parser<'s, 'r>) s = p %& rep p +>= (fun (h, t) -> h::t) .| ret [] <| s
let chainl (pi:Parser<'s, 'r>) (pf:Parser<'s, 'r->'r->'r>) =
    pi %& rep (pf %& pi) +>= fun (h, t) -> List.fold (fun a (f, b) -> f a b) h t
let chainr (pi:Parser<'s, 'r>) (pf:Parser<'s, 'r->'r->'r>) =
    rep (pi %& pf) %& pi +>= fun (b, l) -> List.foldBack (fun (a, f) b-> f a b) b l

//opt
let opt (p:Parser<'s, 'r>) = p %| ret () +>= function | Left r -> Some r | Right _ -> None

//arg
let ( **+) = (%&) //right
let ( **.) pa pb = pa **+ pb **+ ret ()//last
let th (x, _) = x
let tt (_, x) = x
let _1 t = t|>th 
let _2 t = t|>tt|>th
let _3 t = t|>tt|>tt|>th
let _4 t = t|>tt|>tt|>tt|>th


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

let parse s =
    match start (s, 0) with
    | Some (r, _) -> r
    | None -> failwith "parse error"

