//top level interaction (pp.114-115)
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

let rec printv v = function
    | TVV _ -> failwith "polymorphic value"
    | TBas "int" -> match v with | VInt n -> print_num n | _ -> failwith "wrong typing"
    | TBas _ -> print_string "<Bas>"
    | TUnit -> let vUnit = v in print_string("()")
    | TProd (t1,t2) ->
        match v with
        | VPair (v1,v2) ->
            print_string "("; printv v1 t1; print_string ","; printv v2 t2; print_string ")"
        | _ -> failwith "wrong typing"
    | TExp (t1,t2) -> print_string "<clsr>"
    | TZero -> failwith "zero-typed value"
    | TCoprod (t1,t2) -> 
        match v with
        | VIn1 v1 -> print_string "(in1^";printv v1 t1;print_string ")"
        | VIn2 v2 -> print_string "(in2^";printv v2 t2;print_string ")"
        | _ -> failwith "wrong typing"
    | TCoexp (t1,t2) -> print_string "<cntx>"

let rec printt = function
    | TVV n -> print_string ("_ABCDEFGHIJKLMNOP".Substring(n, 1))
    | TBas s -> print_string s
    | TUnit -> print_string "unit"
    | TProd (t1,t2) -> print_string "("; printt t1; print_string "*"; printt t2; print_string ")"
    | TExp (t1,t2) -> print_string "["; printt t1; print_string "->"; printt t2; print_string "]"
    | TZero -> print_string "null"
    | TCoprod (t1,t2) -> print_string "("; printt t1; print_string "+"; printt t2; print_string ")"
    | TCoexp (t1,t2) -> print_string "["; printt t1; print_string "<-"; printt t2; print_string "]"

let prep s =
    let e = parseE s (vtop (!ir)) in
        let (n,t1) = infer e (fun x -> let (_,(m,t)) = !ir x in (m,tterm t)) in
            (e, (n, termt t1))

let printvt v t = printv v t; print_string " : "; printt t; printfn ""

let def x v nt =
    let oir = !ir in ir := (fun i -> if i = x then (RVal v, nt) else oir i)

let z s =
    let (e,(_,t)) = prep s in let r = eval e in
        printvt r t
let zd x s =
    let (e,(n,t))=prep s in let r = eval e in
        def x r (n,t); print_string ("defined " + x + " = "); printvt r t
//SLCをSCLに変換してから実行する部分は未実装
//let m s =
//    let (e,(_,t))=prep s in let r = evalm (transE e) in
//        printvt r t
//and md x s =
//    let (e,(n,t))=prep s in let r = evalm (transE e) in
//        def x r (n,t); print_string ("defined "+ x + " = "); printvt r t
