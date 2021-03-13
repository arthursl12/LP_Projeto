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

fun run (expr:expr) : string =
    (let 
        val e = []
        val t = teval expr e
    in
        "Result"
    end)
    handle Impossible => "Impossible"
        |  NoMatchResults => "Match";

print "TESTES!\n";
run (fromString ("var x = 9; x + 3"));