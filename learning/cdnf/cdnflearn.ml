
(*let (nvars, target) = BoolFormula.from_dimacs stdin*)

(*
let print_assignment assignment =
  let helper i b =
    if i = 0 then () else print_int (if b then 1 else 0) in
  Array.iteri helper assignment

let _ = 
  match Oracle.is_satisfiable_with_assumption nvars target [| |] with
    | None -> print_endline "NO"
    | Some assignment -> 
        print_assignment assignment; print_newline ()

let is_member assignments =
  match Oracle.is_satisfiable_with_assumption nvars target assignments with
    | None -> Query.NO
    | Some _ -> Query.YES

let is_comember assignments =
  let not_target = BoolFormula.Not target in
    match Oracle.is_satisfiable_with_assumption nvars not_target assignments 
    with
      | None -> Query.NO
      | Some _ -> Query.YES

let is_equivalent n conj =
  let conjecture = BoolFormula.from_boolformula_t conj in
  match Oracle.is_equivalent nvars target conjecture with
    | None -> Query.EQ
    | Some assignment -> 
        let counterexample = Array.sub assignment 0 (succ n) in
          Query.CE counterexample *)


let mode = ref (0)

(*let _ = Arg.parse [] (fun str -> mode := int_of_string str)
  "Usage: learn.ext [0|1|2|3]?"

let _ = if !mode < 0 || !mode > 3 then
    (print_string "Usage: learn.ext [0|1|2|3]?"; exit (-1)) *)

let learn nvar is_member is_equivalent = 
  let f = match !mode with
    | 0 -> 
      (match Cdnfp.go nvar is_member is_equivalent with
        | Some f -> f | None -> assert false)
    | 1 -> Cdnfp.gox 1 is_member is_equivalent
    | 2 -> assert false (*Cdnfp.goxx 1 is_member is_comember is_equivalent*)
    | 3 -> Cdnfp.goxxx 1 is_member is_equivalent
    | _ -> assert false in
	(*let _ = 
  	(BoolFormula.print (BoolFormula.from_boolformula_t f);
   	print_newline ()) in*) f
