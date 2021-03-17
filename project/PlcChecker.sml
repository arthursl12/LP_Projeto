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
    |   _ => (
            TextIO.output(TextIO.stdOut, "Match no teval\n");
            raise NoMatchResults
        )
    ;

