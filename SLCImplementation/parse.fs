//parser for SLC terms (CBV recursion) (pp.104-106)
[<FunScript.JS>]
module parse

open form
open slc

type varkind = KVal | KCnt

let rec parseE s r =
    match s with
    | SNum n -> ECst (CInt n)
    | SIde v -> (match r v with KVal -> EIde v | KCnt -> EFnc (parseF s r))
    | SAdd (n,m) -> EUp ((FPrm "+"), EPair ((parseE n r),(parseE m r)))
    | SSub (SNum 0,m) -> EUp ((FPrm "~"), (parseE m r))
    | SSub (n,m) -> EUp ((FPrm "-"), EPair ((parseE n r),(parseE m r)))
    | SMul (n,m) -> EUp ((FPrm "*"), EPair ((parseE n r),(parseE m r)))
    | SEql (n,m) -> EUp ((FPrm "="), EPair ((parseE n r),(parseE m r)))
    | SUp (f,e) -> EUp ((parseF f r),(parseE e r))
    | SPar [] -> EUnit
    | SPar [e] -> parseE e r
    | SPar [e1;e2] -> EPair ((parseE e1 r),(parseE e2 r))
    | SBrc [f] -> EFnc (parseF f r)
    | SLft _ -> EFnc (parseF s r)
    | SRgt _ -> EFnc (parseF s r)
    | SDwn _ -> EFnc (parseF s r)
    | SRec _ -> EFnc (parseF s r)
    | _ -> failwith "bad E syntax"
and parseC s r =
    match s with
    | SIde v -> (match r v with KVal -> CFnc (parseF s r) | KCnt -> CIde v)
    | SDwn (c,f) -> CDwn ((parseC c r),(parseF f r))
    | SBrc [] -> CZero
    | SBrc [c] -> parseC c r
    | SBrc [c1;c2] -> CCase ((parseC c1 r),(parseC c2 r))
    | SRec (y,c) -> let (r1,py) = parseY y r in CRec (py,parseC c r1)
    | SPar [f] -> CFnc (parseF f r)
    | SLft _ -> CFnc (parseF s r)
    | SRgt _ -> CFnc (parseF s r)
    | SUp _ -> CFnc (parseF s r)
    | _ -> failwith "bad C syntax"
and parseF s r =
    match s with
    | SRgt (x,e) -> let (r1,px) = parseX x r in FRgt (px,parseE e r1)
    | SLft (y,c) -> let (r1,py) = parseY y r in FLft (py,parseC c r1)
    | SPar [f] -> parseF f r
    | SIde v -> (match r v with KVal -> FExp (EIde v) | KCnt -> FCnt (CIde v))
    | SUp _ -> FExp (parseE s r)
    | SDwn _ -> FCnt (parseC s r)
    | SRec _ -> FCnt (parseC s r)
    | _ -> failwith "bad F syntax"
and parseX s r =
    match s with
    | SIde v -> ((fun x->if x=v then KVal else r x), (XVar v))
    | SPar [] -> (r, XUnit)
    | SPar [x] -> parseX x r
    | SPar [x1;x2] ->
        let (r1,p1) = parseX x1 r in let (r2,p2) = parseX x2 r1 in
            (r2, XPair (p1,p2))
    | _ -> failwith "bad X syntax"
and parseY s r =
    match s with
    | SIde v -> ((fun y->if y=v then KCnt else r y), (YVar v))
    | SBrc []-> (r, YZero)
    | SBrc [y] -> parseY y r
    | SBrc [y1;y2] ->
        let (r1,p1) = parseY y1 r in let (r2,p2) = parseY y2 r1 in
            (r2, YCase (p1,p2))
    | _ -> failwith "bad Y syntax"
