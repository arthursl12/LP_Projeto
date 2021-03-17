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
    |   (Prim2 (f, e1, e2)) =>(
            TextIO.output(TextIO.stdOut, "eval: Prim2\n");
            let 
                val t1 = eval e1 st
                val t2 = eval e2 st
            in
                case (f,t1,t2) of 
                    ("+", IntV v1, IntV v2) => IntV(v1+v2)
                |   ("-", IntV v1, IntV v2) => IntV(v1-v2)
                |   ("*", IntV v1, IntV v2) => IntV(v1*v2)
                |   ("/", IntV v1, IntV v2) => IntV(v1 div v2)
                |   ("<", IntV v1, IntV v2) => BoolV(v1 < v2)
                |   ("<=", IntV v1, IntV v2) => BoolV(v1 <= v2)
                |   ("&&", BoolV v1, BoolV v2) => BoolV(v1 andalso v2)
                |   ("=", v1, v2) => BoolV(v1 = v2)
                |   ("!=", v1, v2) => BoolV(v1 <> v2)
                |   (";", _, _) => t2
                |   ("::", a, SeqV b) => SeqV (a::b)
                |   _ => raise ValueNotFoundInMatch
            end
        )
    |   _ => (
            TextIO.output(TextIO.stdOut, "Match no eval\n");
            raise NoMatchResults
        )
    ;