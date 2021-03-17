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
        (Var x) => (
            TextIO.output(TextIO.stdOut, "teval: Var\n");
            lookup st x
        )
    |   (ConI i) => (
            TextIO.output(TextIO.stdOut, "teval: Int\n");
            IntT
        )
    |   (Let (x, e1, e2)) => (
            TextIO.output(TextIO.stdOut, "teval: Let variáveis\n");
            let 
                val t1 = teval e1 st
            in 
                teval e2 ((x,t1)::st)
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
                |   ("!=", v1, v2) => BoolT
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
                
                (* Verifica se o tipo dos resultados são iguais*)
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
                else
                    t_Res
            end
        )
    |   _ => (
            TextIO.output(TextIO.stdOut, "Match no teval\n");
            raise NoMatchResults
        )
    ;

