//SLC/SCL type system (pp.109-110)
module typesys
open unify

type Type =
    | TVV of int
    | TBas of string
    | TUnit
    | TProd of Type * Type
    | TExp of Type * Type
    | TZero
    | TCoprod of Type * Type
    | TCoexp of Type * Type

let rec tterm ty =
    match ty with
    | TVV n -> TVar n
    | TBas t -> TFun(t, [])
    | TUnit -> TFun("unit", [])
    | TProd (t1,t2) -> TFun("*", [tterm t1; tterm t2])
    | TExp (t1,t2) -> TFun("->", [tterm t1; tterm t2])
    | TZero -> TFun("zero", [])
    | TCoprod (t1,t2) -> TFun("+", [tterm t1; tterm t2])
    | TCoexp (t1,t2) -> TFun("<-", [tterm t1; tterm t2])

let rec termt term =
    match term with
    | TVar n -> TVV n
    | TFun ("unit", []) -> TUnit
    | TFun ("*", [t1;t2]) -> TProd (termt t1,termt t2)
    | TFun ("->", [t1;t2]) -> TExp (termt t1,termt t2)
    | TFun ("zero", []) -> TZero
    | TFun ("+", [t1;t2]) -> TCoprod (termt t1, termt t2)
    | TFun ("<-", [t1;t2]) -> TCoexp (termt t1, termt t2)
    | TFun (b, []) -> TBas b
    | TFun (s, _) -> failwith ("termt: unknown type constructor " + s)
