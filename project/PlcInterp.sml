(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

(* TODO: tipos dos argumentos de eval *)
fun eval (e: expr) (st:plcVal env) : plcVal = 
    case e of
        (Var x) => lookup st x
    |   (ConI i) => IntV(i)
    |   (ConB b) => BoolV(b)
    |   (Let (x, e1, e2)) =>
            let 
                val t1 = eval e1 st
            in 
                eval e2 ((x,t1)::st)
            end
    |   (ESeq (t)) => SeqV []
    |   (If(e1, e2, e3)) => (
            let
                val v1 = eval e1 st
            in
                if v1 = BoolV(true) then eval e2 st else eval e3 st
            end
        )
    |   (List lst) => (
        let
            fun auxList [] : plcVal list = []
            |   auxList [t] = [eval t st]
            |   auxList (a::xs) = (eval a st) :: auxList(xs)
            val t1 = auxList(lst)
        in
            ListV (t1)
        end
    )
    |   (Item(i, e)) => (
        let 
            val lst_e = eval e st
            fun auxItem ([],n) : plcVal = raise Impossible
            |   auxItem ([t],1) = t
            |   auxItem (a::xs,1) = a
            |   auxItem (a::xs,n) = auxItem(xs,n-1)
        in  
            case lst_e of 
                (ListV lst) => (
                    auxItem(lst,i)
                )
                |   _ => raise OpNonList
        end
    )
    |   (Prim1(f, e1)) => (
            let 
                val v1 = eval e1 st
            in
                case (f, v1) of 
                    ("-", IntV i) => IntV(i * (~1))
                |   ("!", BoolV b) => BoolV(not(b))
                |   ("print", _) => (
                    TextIO.output(TextIO.stdOut, val2string(v1) ^ "\n");
                    ListV []
                    )
                |   ("hd", SeqV lst) => hd(lst)
                |   ("tl", SeqV lst) => SeqV(tl(lst))
                |   ("ise", SeqV lst) => BoolV(lst = [])
                |   _ => raise Impossible
            end
        )
    |   (Letrec (f, p_tp, p_name, ret_tp, e_corpo, call_e)) => eval call_e ((f, Clos(f, p_name, e_corpo, st))::st)
    |   (Prim2 (f, e1, e2)) =>(
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
                |   (";", a, b) => b
                |   ("::", a, SeqV b) => SeqV (a::b)
                |   _ => raise Impossible
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

            val m_None = hasNone lst
        in
            if #1(m_Some) then
                eval (#2(m_Some)) st
            else if #1(m_None) then
                eval (#2(m_None)) st
            else
                raise ValueNotFoundInMatch
        end
    )
    |   (Call (n, par)) => (
            case n of
                (Var f) => (
                    let
                        val fv = (lookup st f)
                    in
                        case fv of
                            (Clos(_, nome, corpo, fSt)) =>
                                let
                                    val ev = (eval par st)
                                    val st1 = (nome, ev) :: (f, fv) :: fSt 
                                    val ret = (eval corpo st1)
                                in
                                    ret
                                end
                        | _ => raise NotAFunc
                    end
                )
            |   (Call (Var n1, par1)) => 
                    let
                        val fv = (lookup st n1)
                    in
                        case fv of
                            (Clos(_, nome, corpo, fSt)) =>
                                let
                                    val ev = (eval par1 st)
                                    val st1 = (nome, ev) :: (n1, fv) :: fSt 
                                    val closure = (eval corpo st1) (* "Closure intermediário" *)
                                in
                                    case closure of
                                        (Clos(_, nome1, corpo1, fSt1)) =>
                                            let
                                                val ev = (eval par st)
                                                val st1 = (nome1, ev) :: fSt1
                                                val ret = (eval corpo1 st1)
                                            in
                                                ret
                                            end
                                    | _ => raise NotAFunc
                                    
                                end
                        | _ => raise NotAFunc
                    end
            |   _ => raise NotAFunc
        )
    |   (Anon (t, nome_par, e)) => Clos("",nome_par, e, st)
    ;