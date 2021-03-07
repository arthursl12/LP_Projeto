functor PlcParserLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PlcParser_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\022\000\003\000\021\000\004\000\020\000\005\000\019\000\
\\006\000\018\000\007\000\017\000\008\000\016\000\009\000\015\000\
\\010\000\014\000\011\000\013\000\012\000\049\000\014\000\012\000\
\\016\000\048\000\017\000\011\000\000\000\
\\001\000\012\000\063\000\013\000\047\000\016\000\062\000\000\000\
\\001\000\012\000\063\000\013\000\047\000\016\000\062\000\017\000\046\000\000\000\
\\001\000\013\000\047\000\017\000\046\000\000\000\
\\001\000\013\000\047\000\018\000\059\000\000\000\
\\001\000\015\000\010\000\019\000\009\000\020\000\008\000\021\000\007\000\
\\022\000\006\000\000\000\
\\001\000\015\000\032\000\016\000\031\000\017\000\030\000\019\000\009\000\
\\020\000\008\000\021\000\007\000\022\000\006\000\023\000\029\000\
\\024\000\028\000\025\000\027\000\000\000\
\\001\000\015\000\051\000\017\000\030\000\023\000\029\000\024\000\028\000\
\\025\000\027\000\000\000\
\\001\000\016\000\045\000\000\000\
\\001\000\016\000\061\000\000\000\
\\001\000\016\000\064\000\000\000\
\\001\000\018\000\054\000\000\000\
\\001\000\018\000\055\000\000\000\
\\001\000\020\000\033\000\000\000\
\\001\000\026\000\000\000\000\000\
\\068\000\002\000\022\000\003\000\021\000\004\000\020\000\005\000\019\000\
\\006\000\018\000\007\000\017\000\008\000\016\000\009\000\015\000\
\\010\000\014\000\011\000\013\000\014\000\012\000\017\000\011\000\000\000\
\\069\000\000\000\
\\070\000\002\000\022\000\003\000\021\000\004\000\020\000\005\000\019\000\
\\007\000\017\000\008\000\016\000\009\000\015\000\010\000\014\000\
\\014\000\012\000\017\000\011\000\000\000\
\\071\000\004\000\020\000\005\000\019\000\017\000\011\000\000\000\
\\072\000\004\000\020\000\005\000\019\000\017\000\011\000\000\000\
\\073\000\017\000\011\000\000\000\
\\074\000\017\000\011\000\000\000\
\\075\000\002\000\022\000\003\000\021\000\004\000\020\000\005\000\019\000\
\\009\000\015\000\010\000\014\000\014\000\012\000\017\000\011\000\000\000\
\\076\000\002\000\022\000\003\000\021\000\004\000\020\000\005\000\019\000\
\\009\000\015\000\010\000\014\000\014\000\012\000\017\000\011\000\000\000\
\\077\000\002\000\022\000\003\000\021\000\004\000\020\000\005\000\019\000\
\\014\000\012\000\017\000\011\000\000\000\
\\078\000\002\000\022\000\003\000\021\000\004\000\020\000\005\000\019\000\
\\014\000\012\000\017\000\011\000\000\000\
\\079\000\002\000\022\000\003\000\021\000\004\000\020\000\005\000\019\000\
\\014\000\012\000\017\000\011\000\000\000\
\\080\000\002\000\022\000\003\000\021\000\004\000\020\000\005\000\019\000\
\\006\000\018\000\007\000\017\000\008\000\016\000\009\000\015\000\
\\010\000\014\000\011\000\013\000\014\000\012\000\017\000\011\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\002\000\022\000\003\000\021\000\004\000\020\000\005\000\019\000\
\\006\000\018\000\007\000\017\000\008\000\016\000\009\000\015\000\
\\010\000\014\000\011\000\013\000\012\000\049\000\014\000\012\000\
\\017\000\011\000\000\000\
\\092\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\013\000\047\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\012\000\063\000\013\000\047\000\000\000\
\\105\000\000\000\
\"
val actionRowNumbers =
"\005\000\029\000\016\000\015\000\
\\034\000\033\000\035\000\030\000\
\\006\000\013\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\008\000\040\000\003\000\
\\000\000\044\000\046\000\045\000\
\\007\000\036\000\006\000\011\000\
\\026\000\027\000\024\000\025\000\
\\023\000\022\000\017\000\021\000\
\\020\000\019\000\018\000\031\000\
\\012\000\007\000\032\000\005\000\
\\004\000\007\000\009\000\002\000\
\\028\000\010\000\043\000\039\000\
\\038\000\042\000\001\000\041\000\
\\047\000\007\000\037\000\049\000\
\\048\000\014\000"
val gotoT =
"\
\\001\000\065\000\003\000\003\000\004\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\024\000\004\000\002\000\006\000\023\000\007\000\022\000\
\\009\000\001\000\010\000\021\000\000\000\
\\000\000\
\\003\000\032\000\004\000\002\000\009\000\001\000\000\000\
\\003\000\033\000\004\000\002\000\009\000\001\000\000\000\
\\003\000\034\000\004\000\002\000\009\000\001\000\000\000\
\\003\000\035\000\004\000\002\000\009\000\001\000\000\000\
\\003\000\036\000\004\000\002\000\009\000\001\000\000\000\
\\003\000\037\000\004\000\002\000\009\000\001\000\000\000\
\\003\000\038\000\004\000\002\000\009\000\001\000\000\000\
\\003\000\039\000\004\000\002\000\009\000\001\000\000\000\
\\003\000\040\000\004\000\002\000\009\000\001\000\000\000\
\\003\000\041\000\004\000\002\000\009\000\001\000\000\000\
\\003\000\042\000\004\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\048\000\007\000\022\000\000\000\
\\000\000\
\\003\000\024\000\004\000\002\000\006\000\051\000\007\000\022\000\
\\008\000\050\000\009\000\001\000\010\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\054\000\007\000\022\000\000\000\
\\000\000\
\\003\000\056\000\004\000\002\000\009\000\001\000\010\000\055\000\000\000\
\\000\000\
\\006\000\058\000\007\000\022\000\008\000\050\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\064\000\007\000\022\000\008\000\063\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 66
val numrules = 38
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NAT of unit ->  (int) | NAME of unit ->  (string)
 | Params of unit ->  ( ( plcType * expr )  list)
 | TypedVar of unit ->  (plcType*expr) | Comps of unit ->  (expr list)
 | Const of unit ->  (expr) | Types of unit ->  (plcType list)
 | AtomType of unit ->  (plcType) | Type of unit ->  (plcType)
 | AppExpr of unit ->  (expr) | AtomExpr of unit ->  (expr)
 | Expr of unit ->  (expr) | Decl of unit ->  (expr)
 | Prog of unit ->  (expr)
end
type svalue = MlyValue.svalue
type result = expr
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 25) => true | _ => false
val showTerminal =
fn (T 0) => "VAR"
  | (T 1) => "PLUS"
  | (T 2) => "MINUS"
  | (T 3) => "MULTI"
  | (T 4) => "DIV"
  | (T 5) => "AND"
  | (T 6) => "EQ"
  | (T 7) => "NEQ"
  | (T 8) => "LTE"
  | (T 9) => "LT"
  | (T 10) => "SEMIC"
  | (T 11) => "COMMA"
  | (T 12) => "ARROW"
  | (T 13) => "DCOLON"
  | (T 14) => "LPAR"
  | (T 15) => "RPAR"
  | (T 16) => "LBRACK"
  | (T 17) => "RBRACK"
  | (T 18) => "NAME"
  | (T 19) => "NAT"
  | (T 20) => "TRUE"
  | (T 21) => "FALSE"
  | (T 22) => "BOOL"
  | (T 23) => "INT"
  | (T 24) => "NIL"
  | (T 25) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 
2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Expr
 as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 0, ( result, Expr1left, Expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.AtomExpr AtomExpr1, AtomExpr1left, 
AtomExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (AtomExpr as AtomExpr1) = AtomExpr1 ()
 in (AtomExpr)
end)
 in ( LrTable.NT 2, ( result, AtomExpr1left, AtomExpr1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("&&",Expr1,Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("+",Expr1,Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("-",Expr1,Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("*",Expr1,Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("/",Expr1,Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("=",Expr1,Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("!=",Expr1,Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<",Expr1,Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<=",Expr1,Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("::",Expr1,Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2(";",Expr1,Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 13, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.NAT NAT1, _,
 _)) :: _ :: ( _, ( MlyValue.Expr Expr1, Expr1left, _)) :: rest671))
 => let val  result = MlyValue.Expr (fn _ => let val  (Expr as Expr1)
 = Expr1 ()
 val  (NAT as NAT1) = NAT1 ()
 in (Item(NAT, Expr))
end)
 in ( LrTable.NT 2, ( result, Expr1left, RBRACK1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.Const Const1, Const1left, Const1right)) :: 
rest671)) => let val  result = MlyValue.AtomExpr (fn _ => let val  (
Const as Const1) = Const1 ()
 in (Const)
end)
 in ( LrTable.NT 3, ( result, Const1left, Const1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.NAME NAME1, NAME1left, NAME1right)) :: 
rest671)) => let val  result = MlyValue.AtomExpr (fn _ => let val  (
NAME as NAME1) = NAME1 ()
 in (Var(NAME))
end)
 in ( LrTable.NT 3, ( result, NAME1left, NAME1right), rest671)
end
|  ( 16, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Comps Comps1,
 _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.AtomExpr (fn _ => let val  (Comps as Comps1) = Comps1 ()
 in (List(Comps))
end)
 in ( LrTable.NT 3, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 17, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expr Expr1, _,
 _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.AtomExpr (fn _ => let val  (Expr as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 3, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 18, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.Const (fn _ => (ConB(true)))
 in ( LrTable.NT 8, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 19, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.Const (fn _ => (ConB(false)))
 in ( LrTable.NT 8, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.NAT NAT1, NAT1left, NAT1right)) :: rest671)
) => let val  result = MlyValue.Const (fn _ => let val  (NAT as NAT1)
 = NAT1 ()
 in (ConI(NAT))
end)
 in ( LrTable.NT 8, ( result, NAT1left, NAT1right), rest671)
end
|  ( 21, ( ( _, ( _, _, RPAR1right)) :: ( _, ( _, LPAR1left, _)) :: 
rest671)) => let val  result = MlyValue.Const (fn _ => (List(nil)))
 in ( LrTable.NT 8, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RPAR1right)) :: _ :: _ :: ( _, ( MlyValue.Type
 Type1, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  
result = MlyValue.Const (fn _ => let val  (Type as Type1) = Type1 ()
 in (ESeq(Type))
end)
 in ( LrTable.NT 8, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Comps (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Expr1 :: (Expr2 :: nil))
end)
 in ( LrTable.NT 9, ( result, Expr1left, Expr2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.Comps Comps1, _, Comps1right)) :: _ :: ( _,
 ( MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result
 = MlyValue.Comps (fn _ => let val  (Expr as Expr1) = Expr1 ()
 val  (Comps as Comps1) = Comps1 ()
 in (Expr :: Comps)
end)
 in ( LrTable.NT 9, ( result, Expr1left, Comps1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.TypedVar TypedVar1, TypedVar1left, 
TypedVar1right)) :: rest671)) => let val  result = MlyValue.Params (fn
 _ => let val  (TypedVar as TypedVar1) = TypedVar1 ()
 in ([TypedVar])
end)
 in ( LrTable.NT 11, ( result, TypedVar1left, TypedVar1right), rest671
)
end
|  ( 26, ( ( _, ( MlyValue.Params Params1, _, Params1right)) :: _ :: (
 _, ( MlyValue.TypedVar TypedVar1, TypedVar1left, _)) :: rest671)) =>
 let val  result = MlyValue.Params (fn _ => let val  (TypedVar as 
TypedVar1) = TypedVar1 ()
 val  (Params as Params1) = Params1 ()
 in (TypedVar :: Params)
end)
 in ( LrTable.NT 11, ( result, TypedVar1left, Params1right), rest671)

end
|  ( 27, ( ( _, ( MlyValue.NAME NAME1, _, NAME1right)) :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.TypedVar (fn _ => let val  (Type as Type1) = Type1 ()
 val  (NAME as NAME1) = NAME1 ()
 in ((Type,Var(NAME)))
end)
 in ( LrTable.NT 10, ( result, Type1left, NAME1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.AtomType AtomType1, AtomType1left, 
AtomType1right)) :: rest671)) => let val  result = MlyValue.Type (fn _
 => let val  (AtomType as AtomType1) = AtomType1 ()
 in (AtomType)
end)
 in ( LrTable.NT 5, ( result, AtomType1left, AtomType1right), rest671)

end
|  ( 29, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Types Types1,
 _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  (Types as Types1) = Types1 ()
 in (ListT(Types))
end)
 in ( LrTable.NT 5, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.Type Type1,
 _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671)) => let val  result
 = MlyValue.Type (fn _ => let val  (Type as Type1) = Type1 ()
 in (SeqT(Type))
end)
 in ( LrTable.NT 5, ( result, LBRACK1left, RBRACK1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (FunT((Type1,Type2)))
end)
 in ( LrTable.NT 5, ( result, Type1left, Type2right), rest671)
end
|  ( 32, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.AtomType (fn _ => (ListT(nil)))
 in ( LrTable.NT 6, ( result, NIL1left, NIL1right), rest671)
end
|  ( 33, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.AtomType (fn _ => (BoolT))
 in ( LrTable.NT 6, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 34, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.AtomType (fn _ => (IntT))
 in ( LrTable.NT 6, ( result, INT1left, INT1right), rest671)
end
|  ( 35, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Type Type1, _,
 _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.AtomType (fn _ => let val  (Type as Type1) = Type1 ()
 in (Type)
end)
 in ( LrTable.NT 6, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Types (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (Type1 :: (Type2 :: nil))
end)
 in ( LrTable.NT 7, ( result, Type1left, Type2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.Types Types1, _, Types1right)) :: _ :: ( _,
 ( MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result
 = MlyValue.Types (fn _ => let val  (Type as Type1) = Type1 ()
 val  (Types as Types1) = Types1 ()
 in (Type :: Types)
end)
 in ( LrTable.NT 7, ( result, Type1left, Types1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PlcParser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun MULTI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMIC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.NAME (fn () => i),p1,p2))
fun NAT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.NAT (fn () => i),p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
end
end
