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

fromString "15";
fromString "true";
fromString "()";
fromString "(6,false)[1]";
fromString "([Bool] [])";
fromString "print x; true";
fromString "3::7::t";
fromString "fn (Int x) => -x end";
fromString "var x = 9; x + 3";
fromString "fun f(Int x) = x; f(1)";
fromString "match x with | 0 -> 1| _ -> -1 end";
fromFile ("example.plc");
use "testParserCases.sml";


fun test ([],n) = true
| test ([h: (string * expr)],n): bool = 
      let
         val v = ((fromString (#1(h))) = #2(h))
      in
         if v = true then
            (
               TextIO.output(TextIO.stdOut, "Teste " ^ Int.toString n ^ "\n");
               v
            )
         else
            (
               TextIO.output(TextIO.stdOut, "!!!Erro!!! Teste " ^ Int.toString n ^ "\n");
               TextIO.output(TextIO.stdOut, #1(h));
               TextIO.output(TextIO.stdOut, "\n");
               v
            )
      end
| test (h::xs, n) = 
      let
         val v = ((fromString (#1(h))) = #2(h))
      in
         if v = true then
            (
               TextIO.output(TextIO.stdOut, "Teste " ^ Int.toString n ^ "\n");
               v andalso test(xs,n+1)
            )
         else
            (
               TextIO.output(TextIO.stdOut, "!!!Erro!!! Teste " ^ Int.toString n ^ "\n");
               TextIO.output(TextIO.stdOut, #1(h));
               TextIO.output(TextIO.stdOut, "\n");
               v andalso test(xs,n+1)
            )
      end
; 
 
test(cases,1);
