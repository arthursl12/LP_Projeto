use "Plc.sml";

fun test ([],n) = true
| test ([h:(string * string)],n): bool = 
      let
         val v = ((run(fromString (#1(h)))) = #2(h))
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
         val v = ((run(fromString (#1(h)))) = #2(h))

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
               v
            )
      end
; 

print "TESTES!\n";
(* run (fromString ("fun rec map ((Int -> Int) f) : ([Int] -> [Int]) = fn ([Int] l) => if ise(l) then l else f(hd(l)) :: map(f)(tl(l)) end; map (2) (10::20::30::([Int] []))")); *)
use "testInterprCases.sml";
test(casesInterpr,1);