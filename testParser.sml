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

print "================File=============\n";
fromFile ("example.plc");
print "================TestCases 0=============\n";


val cases =
  (
    let val s = "0";
        val e = ConI 0
    in
        (s, e)
    end
  ) ::
  [ (
    let val s =
    "fun rec map ((Int -> Int) f) : ([Int] -> [Int]) = fn ([Int] l) => if ise(l) then l else f(hd(l)) :: map(f)(tl(l)) end; map (fn (Int x) => 2*x end) (10::20::30::([Int] []))";
        val e = Letrec ("map",FunT (IntT,IntT), "f", FunT (SeqT IntT,SeqT IntT), Anon(SeqT IntT, "l",If(Prim1 ("ise",Var "l"),Var "l",Prim2("::",Call (Var "f",Prim1 ("hd",Var "l")),Call (Call (Var "map",Var "f"),Prim1 ("tl",Var "l"))))),Call(Call (Var "map",Anon (IntT, "x",Prim2 ("*",ConI 2,Var "x"))),Prim2 ("::",ConI 10,Prim2 ("::",ConI 20,Prim2 ("::",ConI 30,ESeq (SeqT IntT))))))
    in
        (s, e)
    end
  ) ];

val v1 = fromString (#1(hd(cases)));
v1 = #2(hd(cases));


print "================TestCases=============\n";
use "testParserCases.sml";

print "================TestFunct=============\n";

cases;
print "================Teste1=============\n";


print "================Teste3=============\n";

#1(hd(cases));
fromString (#1(hd(cases)));
val v1 = fromString (#1(hd(cases)));
print "================Teste4=============\n";

print "================Teste2=============\n";
v1 = #2(hd(cases));
print "================Teste6=============\n";


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
(* Try to add a systematic way of using the test cases in
   testParserCases to stress test your parser *)
