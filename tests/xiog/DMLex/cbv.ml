datatype lamexp with nat =
    {n:nat} One(n+1)
  | {n:nat} Shift(n+1) of lamexp(n)
  | {n:nat | n > 0} Lam(n-1) of lamexp(n)
  | {n:nat} App(n) of lamexp(n) * lamexp(n)

datatype closure = {n:nat} Clo of lamexp(n) * closure list(n)

fun evaluate (e) =
    let
        fun eval (One, c :: _) = c
          | eval (Shift e, _ :: env) = eval (e, env)
          | eval (e as Lam _, env) = Clo (e, env)
          | eval (App (e1, e2), env) =
            let
                val Clo (Lam body, env') = eval (e1, env)
                val clo = eval (e2, env)
            in
                eval (body, clo :: env')
            end
        withtype {n:nat} lamexp(n) * closure list(n) -> closure
    in
        eval (e, [])
    end
withtype lamexp(0) -> closure