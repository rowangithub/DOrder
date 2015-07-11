datatype lamexp with (sb, nat) =
    {n:nat | n > 0} One(0,n)
  | {n:nat} Int(0,n) of int
  | {b1:sb,b2:sb,b3:sb,n:nat}
    Ifz(0,n) of lamexp(b1,n) * lamexp(b2,n) * lamexp(b3,n)
  | {b1:sb,b2:sb,n:nat} Add(0,n) of lamexp(b1,n) * lamexp(b2,n)
  | {b1:sb,b2:sb,n:nat} Sub(0,n) of lamexp(b1,n) * lamexp(b2,n)
  | {b1:sb,b2:sb,n:nat} Mul(0,n) of lamexp(b1,n) * lamexp(b2,n)
  | {b1:sb,b2:sb,n:nat} Div(0,n) of lamexp(b1,n) * lamexp(b2,n)
  | {b:sb,n:nat} Shift(0,n+1) of lamexp(b,n)
  | {b:sb,n:nat | n >= 1} Lam(1,n-1) of lamexp(b,n)
  | {b1:sb,b2:sb,n:nat} App(0,n) of lamexp(b1,n) * lamexp(b2,n)
  | {b:sb,n:nat | n >= 2} Fix(1,n-2) of lamexp(b,n)

datatype closure =
    Cint of int
  | {n:nat} Cfun of lamexp(1,n) * closure list(n)

fun evaluate (e) =
    let
        fun eval (One, c :: _) = c
	  | eval (e as Int n, _) = Cint n
	  | eval (Ifz (e0, e1, e2), env) =
	    let
		val Cint n = eval (e0, env)
	    in
		if n = 0 then eval (e1, env) else eval (e2, env)
	    end
	  | eval (Add (e1, e2), env) =
	    let
		val Cint n1 = eval (e1, env)
		val Cint n2 = eval (e2, env)
	    in
		Cint (n1 + n2)
	    end
	  | eval (Sub (e1, e2), env) =
	    let
		val Cint n1 = eval (e1, env)
		val Cint n2 = eval (e2, env)
	    in
		Cint (n1 - n2)
	    end
	  | eval (Mul (e1, e2), env) =
	    let
		val Cint n1 = eval (e1, env)
		val Cint n2 = eval (e2, env)
	    in
		Cint (n1 * n2)
	    end
	  | eval (Div (e1, e2), env) =
	    let
		val Cint n1 = eval (e1, env)
		val Cint n2 = eval (e2, env)
	    in
		Cint (n1 / n2)
	    end
          | eval (Shift e, _ :: env) = eval (e, env)
          | eval (e as Lam _, env) = Cfun (e, env)
          | eval (App (e1, e2), env) =
            let
                val clo1 = eval (e1, env)
                val clo2 = eval (e2, env)
            in
		case clo1 of
		    Cfun(Lam body, env') => eval (body, clo2 :: env')
		  | Cfun(Fix body, env') => eval (body, clo2 :: clo1 :: env')
            end
	  | eval (e as Fix _, env) = Cfun (e, env)
        withtype {b:sb,n:nat} lamexp(b,n) * closure list(n) -> closure
    in
        eval (e, [])
    end
withtype {b:sb} lamexp(b,0) -> closure

val two = Shift (One: lamexp (0,1))

val fact =
    Fix (Ifz ((One:lamexp(0,2)),
	      Int 1, Mul (App (two, Sub (One, Int 1)), One)))

val ans = evaluate (App (fact, Int 10))


