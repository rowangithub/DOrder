

(*let f x = match x with [] -> 1 in 2;;*)

match 1::2::[] with [] -> 1 | x1::x2 -> 2;;

(* below gives an exception in unify *)
(*let f x = match x with [] -> 1 | x1::x2 -> x2 in f 4::5;;*)
