val casesInterpr =
  (
    let val s = "0";
        val e = "0 : Int"
    in
        (s, e)
    end
  ) ::
    (
    let val s = "1 + 1";
        val e = "2 : Int"
    in
        (s, e)
    end
  ) ::
    (
    let val s = "10 + 3";
        val e = "13 : Int"
    in
        (s, e)
    end
  ) ::
      (
    let val s = "10 * 3";
        val e = "30 : Int"
    in
        (s, e)
    end
  ) ::
    (
    let val s = "-10 - 3";
        val e = "~13 : Int"
    in
        (s, e)
    end
  ) ::
    (
    let val s = "-10 * 3";
        val e = "~30 : Int"
    in
        (s, e)
    end
  ) ::
    (
    let val s = "10 / 3";
        val e = "3 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "5+3*4";
        val e = "17 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "-3 < 4";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "!(3 = 4)";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "3+1 = 4 && 4 <= 3";
        val e = "false : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "if 3 = 2 then 0 else 1 + 4";
        val e = "5 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "3 + if 3 = 2 then 0 else 1";
        val e = "4 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "4; true";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "4 * (true; 6)";
        val e = "24 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "( )";
        val e = "() : Nil"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "1 < 1";
        val e = "false : Bool"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "1 < 10";
        val e = "true : Bool"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "10 < 1";
        val e = "false : Bool"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "1 <= 1";
        val e = "true : Bool"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "1 <= 10";
        val e = "true : Bool"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "10 <= 1";
        val e = "false : Bool"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "(10 <= 1) && (3 != 4)";
        val e = "false : Bool"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "(1 <= 10) && (3 = 3)";
        val e = "true : Bool"
    in
        (s, e)
    end
  )
    ::
  (
    let val s = "(1,true,3)[1]";
        val e = "1 : Int"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "(1,true,3)[2]";
        val e = "true : Bool"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "(1,true, ([Bool] []))[3]";
        val e = "[] : [Bool]"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "([Bool] [])";
        val e = "[] : [Bool]"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "false::true::([Bool] [])";
        val e = "[false, true, ] : [Bool]"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "hd (false::true::([Bool] []))";
        val e = "false : Bool"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "tl (false::true::([Bool] []))";
        val e = "[true, ] : [Bool]"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "ise ([Bool] [])";
        val e = "true : Bool"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "ise (false::true::([Bool] []))";
        val e = "false : Bool"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "print 9; 3 + 3";
        val e = "6 : Int"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "var x = (45*2); x + 3";
        val e = "93 : Int"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "var x = 4; print x; {var y = 6; print y }";
        val e = "() : Nil"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "var x = 45/15; match x with | 0 -> 1 | _ -> -1 | 3 -> 2 end";
        val e = "2 : Int"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "var f = fn (Int x) => x*x end; f 5";
        val e = "25 : Int"
    in
        (s, e)
    end
  )
  ::
  (
    let val s = "fun rec f (Int x) : Int = if x <= 0 then 1 else x * f(x-1); f(5)";
        val e = "120 : Int"
    in
        (s, e)
    end
  )
::
  (
    let val s = "fun rec map ((Int -> Int) f) : ([Int] -> [Int]) = fn ([Int] l) => if ise(l) then l else f(hd(l)) :: map(f)(tl(l)) end; map (fn (Int x) => 2*x end) (10::20::30::([Int] []))";
        val e = "[20, 40, 60, ] : [Int]"
    in
        (s, e)
    end
  )
  
  ::
  (
    let val s = "(1,false,())";
        val e = "(1, false, (), ) : (Int, Bool, Nil, )"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "(1,(2,3),4)";
        val e = "(1, (2, 3, ), 4, ) : (Int, (Int, Int, ), Int, )"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "(true,false)[1]";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
   (
    let val s = "((3,4), false)[1][2]";
        val e = "4 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "1 + {3}";
        val e = "4 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "print false";
        val e = "() : Nil"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "print (1 - 3)";
        val e = "() : Nil"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Int] [])";
        val e = "[] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Bool] [])";
        val e = "[] : [Bool]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Nil] [])";
        val e = "[] : [Nil]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([[Int]] [])";
        val e = "[] : [[Int]]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Int -> Nil] [])";
        val e = "[] : [Int -> Nil]"
    in
        (s, e)
    end
  )::
  (
    let val s = "([[Int -> Int -> Bool]] [])";
        val e = "[] : [[Int -> Int -> Bool]]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([(Nil, Int, Bool)] [])";
        val e = "[] : [(Nil, Int, Bool, )]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "1 :: ([Int] [])";
        val e = "[1, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "1 :: 2 :: ([Int] [])";
        val e = "[1, 2, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "(1,2) :: (3,4) :: ([(Int,Int)] [])";
        val e = "[(1, 2, ), (3, 4, ), ] : [(Int, Int, )]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "hd (1 :: 2 :: ([Int] []))";
        val e = "1 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "tl (1 :: 2 :: ([Int] []))";
        val e = "[2, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "ise([Int] [])";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "ise(true::([Bool] []))";
        val e = "false : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var x = 4; x+1";
    val e = "5 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "{var x = 4; x+1}";
        val e = "5 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var x = 4; var y = 6; x + y";
        val e = "10 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var x = 4; print x; {var y = 6; print y }";
        val e = "() : Nil"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "1 + {var tmp = 9; tmp + tmp}";
        val e = "19 : Int"
    in
        (s, e)
    end
  )  ::
   (
    let val s = "var a = (3,4); a[1] < a[2]";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) :: 
  (
    let val s = "var e = ([Bool] []); true::false::e";
        val e = "[true, false, ] : [Bool]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fn (Int x) => x end";
        val e = "<fun> : Int -> Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var f = fn (Int x) => x end; f";
        val e = "<fun> : Int -> Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var f = fn (Int x) => x end; f(10)";
        val e = "10 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f (Int x) = x; f";
        val e = "<fun> : Int -> Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f (Int x) = {fun g(Int y) = x+y; g}; f(3)(4)";
        val e = "7 : Int"
    in
        (s, e)
    end
  )  ::
  (
    let val s = "fun f (Int x) = fn (Int y) => x+y end; f(3)(4)";
        val e = "7 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = (
    "fun f (Int -> Bool g) = if g(1) then 10 else 11;"
    ^ "fun h (Int x) = 0 < x;"
    ^ "f(h)");
        val e = "10 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun rec f (Int x) : Int = if x <= 0 then 1 else x + f(x-1); f(5)";
        val e = "16 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun rec pr(Int x): Nil = if x <= 0 then print(0) else { print(x); pr(x-1) }; pr(5)";
        val e = "() : Nil"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun rec len([Int] l): Int = if ise(l) then 0 else 1 + len(tl(l)); len(1::2::([Int] []))";
        val e = "2 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fn (Int x, Int y) => x - y end";
        val e = "<fun> : (Int, Int, ) -> Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f(Int x, Int y) = x - y; f(5,4)";
        val e = "1 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var p = (1,3); fun f(Int x, Int y) = x - y; f(p)";
        val e = "~2 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f(Int x, Int y, Int z) = x - y * z ; f(5,4,2)";
        val e = "~3 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun rec mem(Int x, [Int] l): Bool = if ise(l) then false else if x = hd(l) then true else mem(x, tl(l)); mem(2, 1::2::([Int] []))";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun inc (Int x) = x + 1; fun add (Int x, Int y) = x + y; fun cadd (Int x) = fn (Int y) => x + y end; var y = add(3, inc(4)); var x = cadd(3)(7-y); var z = x * 3; fun rec fac (Int n) : Int = match n with | 0 -> 1 | 1 -> 1 | _ -> n * fac(n - 1) end; print x; print y; x :: y :: z :: fac(z) :: ([Int] [])";
        val e = "[2, 8, 6, 720, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f(Int x, Bool b) = match b with | true -> {x + 1} | _    -> x end; f(3,true)";
        val e = "4 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var E = ([Int] []); fun reverse ([Int] l) = { fun rec rev ([Int] l1, [Int] l2): [Int] = if ise(l1) then l2 else rev(tl(l1), hd(l1)::l2); rev(l, E) }; reverse (1::2::3::E)";
        val e = "[3, 2, 1, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var E = ([Int] []); fun reverse ([Int] s) = { fun rec rev ([Int] s1, [Int] s2): [Int] = match s1 with | E -> s2 | _ -> { var h = hd(s1); var t = tl(s1); rev(t, h::s2) } end; rev(s, E) }; reverse (1::2::3::E)";
        val e = "[3, 2, 1, ] : [Int]"
    in
        (s, e)
    end
  ) ::
   (
    let val s = "fun rec map ((Int -> Int) f) : ([Int] -> [Int]) = fn ([Int] l) => if ise(l) then l else f(hd(l)) :: map(f)(tl(l)) end; map (fn (Int x) => 2*x end) (10::20::30::([Int] []))";
        val e = "[20, 40, 60, ] : [Int]"
    in
        (s, e)
    end
  ) ::

  (
    let val s = "if 11 = 12 then 1 else 0";
        val e = "0 : Int"
    in
        (s, e)
    end
  ) 
  ::

  (
    let val s = "var b = 1 = 2; if b then 3 else 4";
        val e = "4 : Int"
    in
        (s, e)
    end
  ) 

  ::

  (
    let val s = "fun rec f1(Int x) : Int = x + 1; f1(12)";
        val e = "13 : Int"
    in
        (s, e)
    end
  ) 
  
  (* 
  Esses abaixo disparam exceções!

  ::

  (
    let val s = "var b = 1 = 2; if b then b else 4";
        val e = "13 : Int"
    in
        (s, e)
    end
  )  
  
  ::
  (
    let val s = "fun f(Bool x) = if x then 11 else 22; f(0)";
        val e = "11 : Int"
    in
        (s, e)
    end
  ) 


 ::
  (
    let val s = "fun rec f (Bool x) : Bool = if x then 11 else 12; f(true)";
        val e = "11 : Int"
    in
        (s, e)
    end
  )   *)

  :: [ (
    let val s = "0";
        val e = "0 : Int"
    in
        (s, e)
    end
  ) ]
  ;
