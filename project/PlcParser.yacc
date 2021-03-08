%%

%name PlcParser

%pos int

(* Tokens *)
%term   VAR
| PLUS | MINUS | MULTI | DIV | AND
| PRINT | NOT | HEAD | TAIL | ISE
| EQ | NEQ | LTE | LT
| IF | THEN | ELSE 
| MATCH | END | WITH
| SEMIC | COMMA | ARROW | ANONARROW | COLON | DCOLON | UNDERLINE | VBAR
| LPAR | RPAR | LBRACK | RBRACK | LBRACE | RBRACE
| NAME of string
| NAT of int
| TRUE | FALSE
| BOOL | INT | NIL
| EOF 
| VAR_TOKEN | FUN | REC | FN

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
    | TypedVar of plcType * string
    | Params of (plcType * string) list
    | Args of (plcType * string) list
    | CondExpr of expr option
    | MatchExpr of (expr option * expr) list

(* Associativity *)
%right SEMIC ARROW
%nonassoc IF
%left ELSE
%left AND
%left EQ NEQ
%left LTE LT
%right DCOLON
%left PLUS MINUS
%left MULTI DIV
%nonassoc NOT HEAD TAIL ISE PRINT FUN
%left LBRACK

(* Where to end parsing *)
%eop EOF
%noshift EOF

(* Start symbol *)
%start Prog

%verbose 



%%

Prog    :   Expr    (Expr)
    |   Decl     (Decl)

Decl    :   VAR_TOKEN NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
    |   FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, makeAnon(Args, Expr), Prog))
    |   FUN REC NAME Args COLON Type EQ Expr SEMIC Prog (makeFun(NAME, Args, Type, Expr, Prog))


Expr    :   AtomExpr (AtomExpr)
    |   AppExpr (AppExpr)
    |   IF Expr THEN Expr ELSE Expr (If(Expr1,Expr2,Expr3))
    |   MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    |   NOT Expr (Prim1("!",Expr))
    |   MINUS Expr (Prim1("-",Expr))
    |   HEAD Expr (Prim1("hd",Expr))
    |   TAIL Expr (Prim1("tl",Expr))
    |   ISE Expr (Prim1("ise",Expr))
    |   PRINT Expr (Prim1("print",Expr))
    |   Expr AND Expr (Prim2("&&",Expr1,Expr2))
    |   Expr PLUS Expr (Prim2("+",Expr1,Expr2))
    |   Expr MINUS Expr (Prim2("-",Expr1,Expr2))
    |   Expr MULTI Expr (Prim2("*",Expr1,Expr2))
    |   Expr DIV Expr (Prim2("/",Expr1,Expr2))
    |   Expr EQ Expr (Prim2("=",Expr1,Expr2))
    |   Expr NEQ Expr (Prim2("!=",Expr1,Expr2))
    |   Expr LT Expr (Prim2("<",Expr1,Expr2))
    |   Expr LTE Expr (Prim2("<=",Expr1,Expr2))
    |   Expr DCOLON Expr (Prim2("::",Expr1,Expr2))
    |   Expr SEMIC Expr (Prim2(";",Expr1,Expr2))
    |   Expr LBRACK NAT RBRACK (Item(NAT, Expr))

AtomExpr:   Const (Const)
    |   NAME    (Var(NAME))
    |   LPAR Comps RPAR (List(Comps))
    |   LPAR Expr RPAR (Expr)
    |   LBRACE Prog RBRACE (Prog)
    |   FN Args ANONARROW Expr END (makeAnon(Args,Expr))

AppExpr :   AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
    |   AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const   :   TRUE    (ConB(true))
    |   FALSE   (ConB(false))
    |   NAT     (ConI(NAT))
    |   LPAR RPAR (List(nil))
    |   LPAR Type LBRACK RBRACK RPAR (ESeq(Type))

Comps   :   Expr COMMA Expr (Expr1 :: (Expr2 :: nil))
    |   Expr COMMA Comps (Expr :: Comps)

MatchExpr   :   END    ([])
    |   VBAR CondExpr ARROW Expr MatchExpr  ((CondExpr,Expr) :: MatchExpr)

CondExpr:   Expr    (SOME(Expr))
    |   UNDERLINE   (NONE)

Args    :   LPAR RPAR ([])
    |   LPAR Params RPAR (Params)

Params  :   TypedVar ([TypedVar])
    |   TypedVar COMMA Params (TypedVar :: Params)

TypedVar    :   Type NAME ((Type,NAME))

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
