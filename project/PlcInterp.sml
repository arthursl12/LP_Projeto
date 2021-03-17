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
    |   (Match (e1, lst)) =>(
        (* Match of expr * (expr option * expr) list *)
        let
            val t1 = eval e1 st
            (* Verifica se e1 (avaliado) casa com algum SOME *)
            fun matchSome [] t = (false, List [])
            |   matchSome [(SOME es, er)] t = (if ((eval es st) = t) then (true, er) else (false, List []))
            |   matchSome [(NONE, er)] t = (false, List [])
            |   matchSome ((NONE, er)::xs) t = (matchSome xs t)
            |   matchSome ((SOME es, er)::xs) t = (if ((eval es st) = t) then (true, er) else (matchSome xs t))
            
            val m_Some = matchSome lst t1
            
            (* Verifica se existe algum NONE *)
            fun hasNone [] = (false, List[])
            |   hasNone [(SOME es, er)] = (false, List[])
            |   hasNone [(NONE, er)] = (true, er)
            |   hasNone ((NONE, er)::xs) = (true, er)
            |   hasNone ((SOME es, er)::xs) = hasNone xs

            (* Acha o NONE, se ele existe
            fun findNone [] = List []
            |   findNone [(SOME es, er)] = List []
            |   findNone [(NONE, er)] = er
            |   findNone ((NONE, er)::xs) = er
            |   findNone ((SOME es, er)::xs) = (findNone xs) *)

            val m_None = hasNone lst
            (* val m_None = findNone(lst) *)
        in
            if #1(m_Some) then
                eval (#2(m_Some)) st
            else if #1(m_None) then
                eval (#2(m_None)) st
            else
                raise ValueNotFoundInMatch
        end
    )
    |   _ => (
            TextIO.output(TextIO.stdOut, "Match no eval\n");
            raise NoMatchResults
        )
    ;