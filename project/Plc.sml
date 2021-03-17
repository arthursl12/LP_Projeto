(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

use "PlcChecker.sml";
use "PlcInterp.sml";

fun run (e:expr) : string =
    (let 
        val t = teval e []
        val res = eval e []
    in
        type2string(t) ^ " " ^ val2string(res)
    end)
    handle Impossible => "Impossible"
        |  NoMatchResults => "Match";

print "TESTES!\n";
print "Testes Aritméticos Básicos\n";
run (fromString ("1 + 1"));
run (fromString ("10 + 3"));
run (fromString ("10 * 3"));
run (fromString ("-10 - 3"));
run (fromString ("-10 * 3"));
run (fromString ("10 / 3"));

print "Testes Booleanos Básicos\n";
run (fromString ("1 < 1"));
run (fromString ("1 < 10"));
run (fromString ("10 < 1"));
run (fromString ("1 <= 1"));
run (fromString ("1 <= 10"));
run (fromString ("10 <= 1"));
run (fromString ("(10 <= 1) && (3 != 4)"));
run (fromString ("(1 <= 10) && (3 = 3)"));

print "Testes Match\n";
run (fromString ("var x = 45/15; match x with | 0 -> 1 | _ -> -1 | 3 -> 2 end"));
(* run (fromString ("var x = 2; match x with | 0 -> 1 | 3 -> 2 end")); *)

print "Testes Call e Anon Simples\n";
run (fromString ("var f = fn (Int x) => x*x end; f 5"));

run (fromString ("var x = 9; x + 3"));