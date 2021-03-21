(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes 
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

(* Retorna true para tipos de igualdade. Segue o definido na especificação *)
fun eqType t =(
    let
        fun eqTypeList [] = true
        | eqTypeList [t] = eqType(t)
        | eqTypeList (h::xs) = eqType(h) andalso eqTypeList(xs)
    in
        case t of
            (IntT) => true
        |   (BoolT) => true
        |   (ListT x) => eqTypeList(x)
        |   (SeqT t) => eqType(t)
        |   _ => false
    end
)
;

(* TODO: tipos dos argumentos de teval *)
fun teval (e:expr) (st: plcType env) : plcType =
    case e of
        (Var x) => lookup st x
    |   (ConI i) => IntT
    |   (ConB b) => BoolT
    |   (ESeq t) => (
            case t of
                (SeqT t1) => t
            |   _ =>  raise EmptySeq 
    )
    
    
    
   
    |   (Let (x, e1, e2)) => 
            let 
                val t1 = teval e1 st
            in 
                teval e2 ((x,t1)::st)
            end
    |   (If (e1, e2, e3)) => (
            let
                val t1 = teval e1 st
                val t2 = teval e2 st
                val t3 = teval e3 st
            in
                if t1 <> BoolT then
                    raise IfCondNotBool
                else if t2 = t3 then 
                    t2 
                else 
                    raise DiffBrTypes
            end
        )
    |   (List lst) => (
        let
            fun auxList [] : plcType list = []
            |   auxList [t] = [teval t st]
            |   auxList (a::xs) = (teval a st) :: auxList(xs)
            val t1 = auxList(lst)
        in
            ListT (t1)
        end
    )

    |   (Item(i, e)) => (
        let 
            val lst_e = teval e st
            fun auxItem ([],n) : plcType = raise ListOutOfRange
            |   auxItem ([t],1) = t
            |   auxItem (a::xs,1) = a
            |   auxItem (a::xs,n) = auxItem(xs,n-1)
        in  
            case lst_e of 
                (ListT lst) => (
                    auxItem(lst,i)
                )
                |   _ => raise OpNonList
        end
    )
    |   (Prim1 (f, e1)) => (
            let 
                val t1 = teval e1 st
            in
                case (f, t1) of 
                    ("-", IntT) => IntT
                |   ("!", BoolT) => BoolT
                |   ("print", _)  => ListT []
                |   ("hd", SeqT t) => t
                |   ("tl", SeqT t) => SeqT(t)
                |   ("ise", SeqT t) => BoolT 
                |   _ => raise UnknownType
            end
        )
    |   (Letrec (f, p_tp, p_name, ret_tp, e_corpo, call_e)) => (
            let
                val tmp = (p_name, p_tp)::st
                val t1 = teval e_corpo ((f, FunT(p_tp,ret_tp))::tmp)
                val t2 = teval call_e ((f, FunT(p_tp,ret_tp))::st)
            in
                if t1 <> ret_tp then 
                    raise WrongRetType
                else 
                    t2

            end
        )
    |   (Prim2 (f, e1, e2)) =>
            let 
                val t1 = teval e1 st
                val t2 = teval e2 st
            in
                case (f,t1,t2) of 
                    ("+", IntT, IntT) => IntT
                |   ("-", IntT, IntT) => IntT
                |   ("*", IntT, IntT) => IntT
                |   ("/", IntT, IntT) => IntT
                |   ("<", IntT, IntT) => BoolT
                |   ("<=", IntT, IntT) => BoolT
                |   ("&&", BoolT, BoolT) => BoolT
                |   ("=", v1, v2) => if eqType(v1) andalso eqType(v2) then BoolT else raise NotEqTypes
                |   ("!=", v1, v2) => if eqType(v1) andalso eqType(v2) then BoolT else raise NotEqTypes
                |   (";", _, _) => t2
                |   ("::", a, SeqT b) => if a = b then SeqT b else raise DiffBrTypes
                |   _ => raise UnknownType
            end
    |   (Match (e1, lst)) =>(
            (* Match of expr * (expr option * expr) list *)
            let
                val t1 = teval e1 st
                (* Verifica se os tipos dos casos com SOME são do mesmo tipo de e1*)
                fun typePattern [] t = true
                |   typePattern [(SOME es, er)] t = (if (teval es st) = t then true else false)
                |   typePattern [(NONE, er)] t = true
                |   typePattern ((NONE, er)::xs) t = (typePattern xs t)
                |   typePattern ((SOME es, er)::xs) t = (((teval es st) = t) andalso (typePattern xs t))
                
                (* 
                Verifica se o tipo dos resultados são iguais.
                Se não for, dispara exceção específica
                *)
                fun typeResult [] = ListT []
                |   typeResult [(SOME es, er)] = teval er st
                |   typeResult [(NONE, er)] = teval er st
                |   typeResult ((NONE, er)::xs) = if ((teval er st) = (typeResult xs)) then
                                                        (teval er st)
                                                    else
                                                        raise MatchResTypeDiff
                |   typeResult ((SOME es, er)::xs) = if ((teval er st) = (typeResult xs)) then
                                                            (teval er st)
                                                        else
                                                            raise MatchResTypeDiff
                val t_Some = typePattern lst t1
                val t_Res = typeResult lst
            in
                if (not t_Some) then
                    raise MatchCondTypesDiff
                else if (t_Res = ListT []) then
                    raise NoMatchResults
                else
                    t_Res
            end
        )
    |   (Call (f, par)) => (
            let 
                val t_Par = teval par st
                val t_F = teval f st
            in
                case t_F of
                    (FunT(t1, t2)) => if t1 = t_Par then t2 else (
                        raise CallTypeMisM
                    )
                |   _ => raise NotFunc
            end
        )
    |   (Anon (t, nome, e)) =>(
        let
            val t_Ret = teval e ((nome, t)::st)
        in
            FunT(t, t_Ret)
        end
        )
    ;

