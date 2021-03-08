(* Plc Parser Aux *)



(* Create the body of a function expression. *)
fun makeFunAux (n: int, xs: (plcType * string) list, e: expr): expr =
    case xs of
    [(t, name)] => Let(name, Item(n, Var "$list"), e)
    |   ((t, name)::xs) => Let(name, Item(n, Var "$list"), makeFunAux(n+1,xs,e))

(* Create a list of plcType *)
fun makeTypeAux [] = []
  | makeTypeAux [(a,b)] = a :: []
  | makeTypeAux ((a,b)::xs) = a :: makeTypeAux(xs)

(* Create the list of arguments of a function. Returns a ListT *)
fun makeType (args: (plcType * string) list): plcType =
    case args of
        [] => ListT[]
    |   [(t, name)] => ListT(makeTypeAux([(t, name)]))
    |   ((t, name)::xs) => ListT(makeTypeAux((t, name)::xs))

(* Create a function expression. *)
fun makeFun (f: string, xs: (plcType * string) list, rt: plcType, e1: expr, e2: expr): expr =
  case xs of
      [] => Letrec(f, ListT [], "()", rt, e1, e2)
    | (t,x)::[] => Letrec(f, t, x, rt, e1, e2)
    | _ =>
      let
        val t = makeType xs
        val e1' = makeFunAux (1, xs, e1)
      in
        Letrec(f, t, "$list", rt, e1', e2)
      end;

(* Create a Anonymus function expression. *)
fun makeAnon (xs:(plcType * string) list, e:expr):expr =
  case xs of
      [] => Anon(ListT [], "()", e)
    | (t,x)::[] => Anon(t,x,e)
    | _ =>
      let
        val t = makeType xs
      in
        let
          val e' = makeFunAux (1, xs, e)
        in
          Anon(t,"$list",e')
        end
      end;
