%%

%name PlcParser

%pos int

(*
CHECK-LIST:
WIP = not fully implemented yet
TODO = to be implemented
OK = fully implemented

Prog    : WIP
Decl    : TODO
Expr    : WIP
AtomExpr: WIP
AppExpr : TODO
Const   : OK
Comps   : OK
MatchExpr   : TODO
CondExpr    : TODO
Args    : TODO
Params  : OK
TypedVar: OK
AtomicType  : OK
Types   : OK
*)

(* Tokens *)
%term   VAR
| PLUS | MINUS | MULTI | DIV
| EQ | NEQ | LTE | LT
| SEMIC | COMMA | ARROW
| LPAR | RPAR | LBRACK | RBRACK
| NAME of string
| NAT of int
| TRUE | FALSE
| BOOL | INT | NIL
| EOF

(* Non-terminals declarations *)
%nonterm Prog of expr
    | Decl of expr
    | Expr of expr
    | AtomExpr of expr
    | AppExpr of expr
    | Type of plcType
    | AtomType of plcType
    | Types of plcType list
    | Const of expr
    | Comps of expr list
    | TypedVar of plcType * expr
    | Params of (plcType * expr) list

(* Associativity *)
%right SEMIC ARROW
%left PLUS MINUS
%left EQ MULTI DIV 
%left RBRACK

(* Where to end parsing *)
%eop EOF
%noshift EOF

(* Start symbol *)
%start Prog

%verbose 

%%

Prog    :   Expr    (Expr)

Expr    :   AtomExpr (AtomExpr)
    |   Expr PLUS Expr (Prim2("+",Expr1,Expr2))
    |   Expr MINUS Expr (Prim2("-",Expr1,Expr2))
    |   Expr MULTI Expr (Prim2("*",Expr1,Expr2))
    |   Expr DIV Expr (Prim2("/",Expr1,Expr2))
    |   Expr EQ Expr (Prim2("=",Expr1,Expr2))
    |   Expr NEQ Expr (Prim2("!=",Expr1,Expr2))
    |   Expr LT Expr (Prim2("<",Expr1,Expr2))
    |   Expr LTE Expr (Prim2("<=",Expr1,Expr2))
    |   Expr LBRACK NAT RBRACK (Item(NAT, Expr))



AtomExpr:   Const (Const)
    |   NAME    (Var(NAME))
    |   LPAR Comps RPAR (List(Comps))
    |   LPAR Expr RPAR (Expr)

Const   :   TRUE    (ConB(true))
    |   FALSE   (ConB(false))
    |   NAT     (ConI(NAT))
    |   LPAR RPAR (List(nil))
    |   LPAR Type LBRACK RBRACK RPAR (ESeq(Type))

Comps   :   Expr COMMA Expr (Expr1 :: (Expr2 :: nil))
    |   Expr COMMA Comps (Expr :: Comps)

Params  :   TypedVar ([TypedVar])
    |   TypedVar COMMA Params (TypedVar :: Params)

TypedVar    :   Type NAME ((Type,Var(NAME)))

Type    :   AtomType (AtomType)
    |   LPAR Types RPAR (ListT(Types))
    |   LBRACK Type RBRACK (SeqT(Type))
    |   Type ARROW Type (FunT((Type1,Type2)))

AtomType:   NIL (ListT(nil))
    |   BOOL    (BoolT)
    |   INT     (IntT)
    |   LPAR Type RPAR (Type)

Types   :   Type COMMA Type (Type1 :: (Type2 :: nil))
    |   Type COMMA Types (Type :: Types)
