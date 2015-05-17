//abstract syntax of the SLC (p.106)
module slc

type var = string
type cst = CInt of int

type exp = 
    | ECst of cst
    | EIde of var
    | EUnit
    | EPair of exp * exp
    | EUp of fnc * exp
    | ERec of pax * exp
    | EFnc of fnc
and cnt =
    | CIde of var
    | CZero
    | CCase of cnt * cnt
    | CDwn of cnt * fnc
    | CRec of pay * cnt
    | CFnc of fnc
and fnc = 
    | FRgt of pax * exp
    | FLft of pay * cnt
    | FPrm of var
    | FExp of exp
    | FCnt of cnt
and pax =
    | XVar of var
    | XPair of pax * pax
    | XUnit
and pay =
    | YVar of var
    | YCase of pay * pay
    | YZero
