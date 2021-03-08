val cases =
  (
    let val s = "0";
        val e = ConI 0
    in
        (s, e)
    end
  ) ::
  [ (
    let val s =
    "fun rec map ((Int -> Int) f) : ([Int] -> [Int]) = fn ([Int] l) => if ise(l) then l else f(hd(l)) :: map(f)(tl(l)) end; map (fn (Int x) => 2*x end) (10::20::30::([Int] []))";
        val e = Letrec ("map",FunT (IntT,IntT), "f", FunT (SeqT IntT,SeqT IntT), Anon(SeqT IntT, "l",If(Prim1 ("ise",Var "l"),Var "l",Prim2("::",Call (Var "f",Prim1 ("hd",Var "l")),Call (Call (Var "map",Var "f"),Prim1 ("tl",Var "l"))))),Call(Call (Var "map",Anon (IntT, "x",Prim2 ("*",ConI 2,Var "x"))),Prim2 ("::",ConI 10,Prim2 ("::",ConI 20,Prim2 ("::",ConI 30,ESeq (SeqT IntT))))))
    in
        (s, e)
    end
  ) ];
