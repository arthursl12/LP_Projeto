(* Infrastructure to run the Plc Front-End *)

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

print "================Início=============\n";

(* 
fromString "()";
fromString "(Int x, Int y)"; *)




fromString "15";
fromString "true";
fromString "()";
fromString "([Bool] [])";
fromString "(6,false)";
fromString "(6,false)[1]";

fromString "1+1";
fromString "1-3*5/3";
fromString "(57-351)";
fromString "(3+5) <= (75*34)";
fromString "(3+5) < (75*34)";
fromString "(3+5) = (75*34)";

fromString "3::7::t";
fromString "1+1; 7+7";

fromString "p && c";
fromString "(1 <= 35) && (5 != p)";



fromString "print x; true";
fromString "ise (6,3,4)";
fromString "(ise () = false)";

fromString "if (x < y) then x else y";
fromString "match x with | 0 -> 1| _ -> -1 end";

fromString "var x = 9; x + 3";
fromString "var x = 4; var y = 6; x + y";


print "================Funções=============\n";
fromString "f(1)";
fromString "fun f(Int x) = x; f(1)";
fromString "fn (Int x) => -x end";
fromString "fun rec f(Int n):Int = if n <= 0 then 0 else n + f(n-1); f(5)";
fromFile ("example.plc");

use "testParserCases.sml"

(* Try to add a systematic way of using the test cases in
   testParserCases to stress test your parser *)
