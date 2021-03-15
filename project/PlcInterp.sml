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
    |   _ => (
            TextIO.output(TextIO.stdOut, "Match no eval\n");
            raise NoMatchResults
        )
    ;