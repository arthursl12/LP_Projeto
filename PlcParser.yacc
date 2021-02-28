%%

%name PlcParser

%pos int

(* Tokens *)
%term   VAR
| PLUS | MINUS | MULTI | DIV | EQ
| SEMIC | COMMA
| LPAR | RPAR
| NAME of string
| CINT of int | CBOOL of bool
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

(* Associativity *)
%right SEMIC
%left EQ PLUS MINUS MULTI DIV

(* Where to end parsing *)
%eop EOF
%noshift EOF

(* Start symbol *)
%start Type



%%

Type    :   AtomType (AtomType)
    |   LPAR Types RPAR (ListT(Types))

AtomType:   NIL (NilT)
    |   BOOL    (BoolT)
    |   INT     (IntT)
    |   LPAR Type RPAR (Type)

Types   :   Type COMMA Type (Type1 :: (Type2 :: nil))
    |   Type COMMA Types (Type :: Types)