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
    |   (ConB b) => (
            TextIO.output(TextIO.stdOut, "teval: Bool\n");
            BoolT
        )
    |   (ESeq e) => (
            TextIO.output(TextIO.stdOut, "teval: ESeq\n");
            e
        )
    |   (Let (x, e1, e2)) => (
            TextIO.output(TextIO.stdOut, "teval: Let variáveis\n");
            let 
                val t1 = teval e1 st
            in 
                teval e2 ((x,t1)::st)
            end
        )
    |   (If (e1, e2, e3)) => (
            TextIO.output(TextIO.stdOut, "teval: If\n");
            let
                val t1 = teval e1 st
                val t2 = teval e2 st
                val t3 = teval e3 st
            in
                if t1 = BoolT andalso t2 = t3 then t2 else raise DiffBrTypes
        )
    |   (Prim1 (f, e1)) =>(
            TextIO.output(TextIO.stdOut, "teval: Prim1\n");
            let 
                val t1 = teval e1 st
            in
                case f of 
                    "-" => if t1 = IntT then IntT else raise DiffBrTypes
                |   "!" => if t1 = BoolT then BoolT else raise DiffBrTypes
                |   "print" => Nil
                |   "hd" => if t1 = SeqT then teval (hd t1) st else raise DiffBrTypes
                |   "tl" => if t1 = SeqT then SeqT else raise DiffBrTypes
                |   "ise" => if t1 = SeqT then BoolT else raise DiffBrTypes
            end
        )
    |   (Prim2 (f, e1, e2)) =>(
            TextIO.output(TextIO.stdOut, "teval: Prim2\n");
            let 
                val t1 = teval e1 st
                val t2 = teval e2 st
            in
                case f of 
                    "+" => if t1 = IntT andalso t2 = IntT then IntT else raise DiffBrTypes
                |    "-" => if t1 = IntT andalso t2 = IntT then IntT else raise DiffBrTypes
                |    "*" => if t1 = IntT andalso t2 = IntT then IntT else raise DiffBrTypes
                |    "/" => if t1 = IntT andalso t2 = IntT then IntT else raise DiffBrTypes
                |    "<" => if t1 = IntT andalso t2 = IntT then BoolT else raise DiffBrTypes
                |    "<=" => if t1 = IntT andalso t2 = IntT then BoolT else raise DiffBrTypes
            end

        )
    |   _ => (
            TextIO.output(TextIO.stdOut, "Match no teval\n");
            raise NoMatchResults
        )
    ;

