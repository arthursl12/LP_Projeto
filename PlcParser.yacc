%%

%name PlcParser

%pos int

(* Tokens *)
%term   VAR
| PLUS | MINUS | MULTI | DIV | EQ
| SEMIC | COMMA
| LPAR | RPAR
| NAME of string
| CINT of int
| EOF
| BOOL | INT | NIL

(* Non-terminals declarations *)
%nonterm Prog of expr
    | Decl of expr
    | Expr of expr
    | AtomExpr of expr
    | AppExpr of expr
    | Type of plcType
    | AtomType of plcType
    | Types of plcType

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

AtomType:   NIL (IntT)
    |   BOOL    (BoolT)
    |   INT     (IntT)
    |   LPAR Type RPAR (Type)

Types   :   Type COMMA Type (Type)
    |   Type COMMA Types (Type)