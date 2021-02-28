%%

%name PlcParser

%pos int

(* Tokens *)
%term   VAR
| PLUS | MINUS | MULTI | DIV | EQ
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

(* Associativity *)
%right SEMIC ARROW
%left EQ PLUS MINUS MULTI DIV 
%left RBRACK

(* Where to end parsing *)
%eop EOF
%noshift EOF

(* Start symbol *)
%start Expr

%verbose 

%%

Expr    :   AtomExpr (AtomExpr)

AtomExpr:   Const (Const)
    |   NAME    (Var(NAME))
    |   LPAR Expr RPAR (Expr)

Const   :   TRUE    (ConB(true))
    |   FALSE   (ConB(false))
    |   NAT     (ConI(NAT))
    |   LPAR RPAR (List(nil))
    |   LPAR Type LBRACK RBRACK RPAR (ESeq(Type))

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