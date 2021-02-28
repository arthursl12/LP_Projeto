(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token


fun keyword(s, lpos, rpos) =
    case s of 
        "Int"   => INT(lpos, rpos)
        | "Bool"  => BOOL(lpos, rpos)
        | "Nil" => NIL(lpos, rpos)
        | _     => NAME(s, lpos, rpos)
        

fun strToInt s =
    case Int.fromString s of
        SOME i => i
    |   NONE => raise Fail ("Could not convert string '" ^ s ^ "' to integer")


(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));

nat = [0-9]+;
name = [a-zA-Z_][a-zA-Z_0-9]*;
whitespace = [\ \t];
%%

\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{name} => (keyword(yytext, yypos, yypos));
{nat} => (CINT(strToInt(yytext),yypos, yypos));
"(" => (LPAR(yypos, yypos));
")" => (RPAR(yypos, yypos));
"," => (COMMA(yypos, yypos));

. => (error("\n***lexer error: bad character ***\n"); raise Fail("Lexer error:"^
        "bad character " ^ yytext));