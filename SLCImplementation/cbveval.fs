//semantic domains for CBV evaluation (p.112)
#if TOJS
[<FunScript.JS>]
#endif
module cbveval
open slc
open typesys

//関数の等価比較のための一意なラベル値をVClosr,Cntに付加するように変更
let id = ref 0
let genId () =
    id := !id + 1
    !id

type 'f labeled = Label of int * 'f

type Val =
    | VInt of int
    | VUnit
    | VPair of Val * Val
    | VIn1 of Val
    | VIn2 of Val
    | VClosr of ((Val->Cnt->Ans) labeled)
    | VContx of Val * Cnt
    static member eq vx vy =
        match (vx, vy) with
        | (VInt i1, VInt i2) -> i1 = i2
        | (VUnit, VUnit) -> true
        | (VPair (v1l, v1r), VPair (v2l, v2r)) -> Val.eq v1l v2l && Val.eq v1r v2r
        | (VIn1 v1, VIn1 v2) -> Val.eq v1 v2
        | (VIn2 v1, VIn2 v2) -> Val.eq v1 v2
        | (VClosr (Label(l1, _)), VClosr (Label(l2, _))) -> l1 = l2
        | (VContx (v1, (Label(l1, _))), VContx (v2, (Label(l2, _)))) -> Val.eq v1 v2 && l1 = l2
        | _ -> false
and Cnt = (Val->Ans) labeled
and Ans =
    | AResult of Val
    | AError

let label f = Label(genId(), f)
let unlabel = function Label(_, f) -> f
let labeledAResult = label AResult

type rvar =
    | RVal of Val
    | RCnt of Cnt
    | RErr
and Env = var -> rvar
and TopEnv = var->(rvar * (int * Type))

let ir = ref ((fun v -> (RErr,(0,TBas "err"))):TopEnv)

let prim p v =
    match (p, v) with
    | ("+", (VPair (VInt n, VInt m))) -> VInt (n+m)
    | ("-", (VPair (VInt n, VInt m))) -> VInt (n-m)
    | ("*", (VPair (VInt n, VInt m))) -> VInt (n*m)
    | ("=", (VPair (a, b))) -> if Val.eq a b then VIn1(VUnit) else VIn2(VUnit)
    | ("~", (VInt n)) -> VInt (- n)
    | _ -> failwith ("unknown primitive: " + p)
