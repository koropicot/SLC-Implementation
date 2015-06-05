//top level interaction (pp.114-115)
[<FunScript.JS>]
module toplev
open parse
open typesys
open cbveval
open slctype
open slcint

let print_string = printf "%s"
let print_num = printf "%d"

let vtop r v =
    match r v with
    | ((RVal _),_)-> KVal
    | ((RCnt _),_)-> KCnt
    | (RErr, _) -> failwith ("Unknown identifier: " + v)

let num_to_str (n:int) = string n

let rec v_to_str v = function
    | TVV _ -> failwith "polymorphic value"
    | TBas "int" -> match v with | VInt n -> num_to_str n | _ -> failwith "wrong typing"
    | TBas _ -> "<Bas>"
    | TUnit -> let vUnit = v in "()"
    | TProd (t1,t2) ->
        match v with
        | VPair (v1,v2) ->
            "("+ v_to_str v1 t1 + "," + v_to_str v2 t2 + ")"
        | _ -> failwith "wrong typing"
    | TExp (t1,t2) -> "<clsr>"
    | TZero -> failwith "zero-typed value"
    | TCoprod (t1,t2) -> 
        match v with
        | VIn1 v1 -> "(in1^" + v_to_str v1 t1 + ")"
        | VIn2 v2 -> "(in2^" + v_to_str v2 t2 + ")"
        | _ -> failwith "wrong typing"
    | TCoexp (t1,t2) -> "<cntx>"

let rec t_to_str = function
    | TVV n -> "_ABCDEFGHIJKLMNOP".Substring(n, 1)
    | TBas s -> s
    | TUnit -> "unit"
    | TProd (t1,t2) -> "(" + t_to_str t1 + "*" + t_to_str t2 + ")"
    | TExp (t1,t2) -> "[" + t_to_str t1 + "->" + t_to_str t2 + "]"
    | TZero -> "null"
    | TCoprod (t1,t2) -> "(" + t_to_str t1 + "+" + t_to_str t2 + ")"
    | TCoexp (t1,t2) -> "[" + t_to_str t1 + "<-" + t_to_str t2 + "]"

let vt_to_str v t = v_to_str v t + " : " + t_to_str t

let prep s =
    let e = parseE s (vtop (!ir)) in
        let (n,t1) = infer e (fun x -> let (_,(m,t)) = !ir x in (m,tterm t)) in
            (e, (n, termt t1))

let def x v nt =
    let oir = !ir in ir := (fun i -> if i = x then (RVal v, nt) else oir i)

//評価結果を出力せず文字列で返す
let z_to_str s =
    let (e,(_,t)) = prep s in let r = eval e in
        vt_to_str r t
let zd_to_str x s =
    let (e,(n,t))=prep s in let r = eval e in
        def x r (n,t); 
        "defined " + x + " = " + vt_to_str r t

//SLCをSCLに変換してから実行する部分は未実装
//let m s =
//    let (e,(_,t))=prep s in let r = evalm (transE e) in
//        printvt r t
//and md x s =
//    let (e,(n,t))=prep s in let r = evalm (transE e) in
//        def x r (n,t); print_string ("defined "+ x + " = "); printvt r t
