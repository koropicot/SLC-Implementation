//CBV interpreter for SLC terms (pp.112-113)
[<FunScript.JS>]
module slcint
open slc
open cbveval

let rec E_ e r k =
    match e with
    | ECst (CInt n) -> (unlabel k) (VInt n)
    | EIde x -> match r x with | RVal v -> (unlabel k) v | _ -> failwith "EIde"
    | EUnit -> (unlabel k) VUnit
    | EPair (e1,e2) -> E_ e1 r (label (fun v1->E_ e2 r (label (fun v2->(unlabel k) (VPair (v1,v2))))))
    | EUp (f,e) -> E_ e r (label (fun v->F_ f r v k))
    | ERec (x,e) -> AError
    | EFnc f -> (unlabel k) (VClosr (label (fun v c->F_ f r v c)))
and C_ c r v =
    match c with
    | CIde y -> match r y with | RCnt k -> (unlabel k) v | _ -> failwith "CIde"
    | CZero -> AError
    | CCase (c1,c2) -> (match v with | VIn1 a -> C_ c1 r a | VIn2 b -> C_ c2 r b | _ -> failwith "")
    | CDwn (c,f) -> F_ f r v (label (fun t->C_ c r t))
    | CRec (y,c) -> let rec k a=C_ c (Y_ y (label k) r) a in k v
    | CFnc f -> match v with | VContx(a,c) -> F_ f r a c | _ -> failwith "CFnc"
and F_ f r v k =
    match f with
    | FPrm p -> (unlabel k) (prim p v)
    | FRgt (x,e) -> E_ e (X_ x v r) k
    | FLft (y,c) -> C_ c (Y_ y k r) v
    | FExp e -> E_ e r (label (fun t->match t with | VClosr f -> (unlabel f) v k | _ -> failwith "FExp"))
    | FCnt c -> C_ c r (VContx(v,k))
and X_ x v r =
    match x with
    | XVar x -> (fun x1->if x=x1 then RVal v else r x1)
    | XPair (x1,x2) -> match v with | VPair(v1,v2) -> X_ x1 v1 (X_ x2 v2 r) | _ -> failwith "XPair"
    | XUnit -> let VUnit = v in r
and Y_ y k r =
    match y with
    | YVar y -> (fun y1->if y=y1 then RCnt k else r y1)
    | YCase (y1,y2) -> 
        let k1=((unlabel k) << VIn1) in let k2=((unlabel k) << VIn2) in
            Y_ y1 (label k1) (Y_ y2 (label k2) r)
    | YZero -> r

let eval e =
    match E_ e (fun x->let (v,_) = !ir x in v) labeledAResult with
    | AResult a -> a
    | _ -> failwith "eval"
