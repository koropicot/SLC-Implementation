//type inferencer for SLC terms (pp.109-110)
#if TOJS
[<FunScript.JS>]
#endif
module slctype
open common
open slc
open unify

let bt = function
    Tuple3(n, t, l) ->
    PT(n+1, TFun("|-", TVar(n+1)::t), List.map (fun (x, y)->(x, TFun("|-", TVar(n+1)::y))) l)

let ext r v =
    fun i -> if i=v then (1, TFun(",", [V1;V_]), V1)
                else let (n,G,T)=r i in (n, TFun(",", [V_;G]), T)

let rec typeE e r =
    match e with
    | ECst (CInt n) -> bt (Tuple3(0, [tCst "int"], []))
    | EIde x -> let (n,G,T)=r x in PT(n, TFun("|-", [G;T]), [])
    | EUnit -> bt (Tuple3(0, [tCst "unit"], []))
    | EPair (e1,e2) -> bt (Tuple3(2, [TFun("*", [V1;V2])], [typeE e1 r,[V1]; typeE e2 r,[V2]]))
    | EUp (f,e) -> bt (Tuple3(2, [V2], [typeF f r,[V1;V2]; typeE e r,[V1]]))
    | EFnc f -> bt (Tuple3(2, [TFun("->",[V1;V2])], [typeF f r,[V1;V2]]))
    | ERec (x,e) ->
        PT(3, TFun("|-", [V1;V2]),
            let (r1,ptx) = typeX x r in [ptx, TFun("|:", [V1;V2;V3]); typeE e r1, TFun("|-", [V3;V2])])
and typeC c r =
    match c with
    | CIde y -> let (n,G,T)=r y in PT(n, TFun("|-", [G;T]), [])
    | CZero -> bt (Tuple3(0, [tCst "zero"], []))
    | CCase (c1,c2) -> bt (Tuple3(2, [TFun("+", [V1;V2])], [typeC c1 r,[V1]; typeC c2 r,[V2]]))
    | CDwn (c,f) -> bt (Tuple3(2, [V1], [typeF f r,[V1;V2]; typeC c r,[V2]]))
    | CFnc f -> bt (Tuple3(2, [TFun("<-",[V2;V1])], [typeF f r,[V1;V2]]))
    | CRec (y,c) ->
        PT(3, TFun("|-", [V1;V2]),
            let (r1,pty) = typeY y r in [pty, TFun("|:", [V1;V2;V3]); typeC c r1, TFun("|-", [V3;V2])])
and typeF f r =
    match f with
    | FPrm "+" -> bt (Tuple3(0, [TFun ("*", [tCst "int"; tCst "int"]); tCst "int"], []))
    | FPrm "-" -> bt (Tuple3(0, [TFun ("*", [tCst "int"; tCst "int"]); tCst "int"], []))
    | FPrm "*" -> bt (Tuple3(0, [TFun ("*", [tCst "int"; tCst "int"]); tCst "int"], []))
    | FPrm "=" -> bt (Tuple3(1, [TFun ("*", [V1;V1]); TFun("+",[tCst "unit";tCst "unit"])], []))
    | FPrm "~" -> bt (Tuple3(0, [tCst "int"; tCst "int"], []))
    | FPrm _ -> failwith "typeF: unknown primitive function"
    | FRgt (x,e) ->
        let (r1,ptx) = typeX x r in
            PT(4, TFun("|-", [V1; V2; V3]),
                [ptx,TFun("|:", [V1;V2;V4]); typeE e r1,TFun("|-", [V4; V3])])
    | FLft (y,c) ->
        let (r1,pty) = typeY y r in
            PT(4, TFun("|-", [V1; V2; V3]),
                [pty,TFun("|:", [V1;V3;V4]); typeC c r1,TFun("|-", [V4; V2])])
    | FExp e -> bt (Tuple3(2, [V1;V2], [typeE e r,[TFun("->", [V1;V2])]]))
    | FCnt c -> bt (Tuple3(2, [V1;V2], [typeC c r,[TFun("<-", [V2;V1])]]))
and typeX x r =
    match x with
    | XVar x -> (ext r x, PT(2, TFun("|:", [V1;V2;TFun(",", [V2;V1])]), []))
    | XUnit -> (r, PT(1, TFun("|:", [V1;tCst "unit";V1]), []))
    | XPair (x1,x2) ->
        let (r1,pt1)=typeX x1 r in let(r2,pt2)=typeX x2 r1 in
            (r2, PT(5, TFun("|:", [V1;TFun("*", [V2;V3]);V4]),
                    [pt1, TFun("|:", [V1;V2;V5]); pt2, TFun("|:", [V5;V3;V4])]))
and typeY y r =
    match y with
    | YVar y -> (ext r y, PT(2, TFun("|:", [V1;V2;TFun(",", [V2;V1])]), []))
    | YZero -> (r, PT(1, TFun("|:", [V1;tCst "zero";V1]), []))
    | YCase (y1,y2) ->
        let (r1,pt1)=typeY y1 r in let(r2,pt2)=typeY y2 r1 in 
            (r2, PT(5, TFun("|:", [V1;TFun("+", [V2;V3]);V4]),
                    [pt1, TFun("|:", [V1;V2;V5]); pt2, TFun("|:", [V5;V3;V4])]))

let infer e gr =
    let (s1,_) = solve (typeE e (fun x->let (n,t)=gr x in (n,V_,t)))
                        (TFun ("|-", [V_; V1])) 1 emptysubst in
        let (l,t) = gen [] (s1 1) in
            (List.length l,t)
