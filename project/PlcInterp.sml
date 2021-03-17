(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

(* TODO: tipos dos argumentos de eval *)
fun eval (e: expr) (st:plcVal env) : plcVal = 
    case e of
        (Var x) => (
            TextIO.output(TextIO.stdOut, "eval: Var\n");
            lookup st x
        )
    |   (ConI i) => (
            TextIO.output(TextIO.stdOut, "eval: Int\n");
            i
        )
    |   (ConB b) => (
            TextIO.output(TextIO.stdOut, "eval: Bool\n");
            b
        )
    |   (Let (x, e1, e2)) => (
            TextIO.output(TextIO.stdOut, "eval: Let variáveis\n");
            let 
                val t1 = eval e1 st
            in 
                eval e2 ((x,t1)::st)
            end
        )
    |   _ => (
            TextIO.output(TextIO.stdOut, "Match no eval\n");
            raise NoMatchResults
        )
    ;