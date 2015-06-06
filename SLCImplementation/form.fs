//syntactic forms recognized by the YACC pre-parser (pp.103-104)
#if TOJS
[<FunScript.JS>]
#endif
module form

type form =
    | SNum of int
    | SIde of string
    | SAdd of form * form
    | SSub of form * form
    | SMul of form * form
    | SUp of form * form
    | SDwn of form * form
    | SEql of form * form
    | SLft of form * form
    | SRgt of form * form
    | SRec of form * form
    | SPar of form list
    | SBrc of form list
// SCLの構文
//    | SSqb of form list
//    | SAnb of form list

// トップレベルの構文
type top =
    | Def of string * form
    | Eval of form
