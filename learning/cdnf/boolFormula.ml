(*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************)

type var_t = int

type lit_t = int

type t = Lit of lit_t | Not of t | And of t array | Or of t array

let is_positive l = let _ = assert (l <> 0) in l > 0

let of_var l = let _ = assert (l <> 0) in abs l

let rec from_boolformula_t f =
  match Cdnfp.get_type f with
    | Cdnfp.Lit -> 
        let cdnf_lit = Cdnfp.get_literal f in
        let cdnf_var = Cdnfp.var_of_literal cdnf_lit in
        Lit (if Cdnfp.positive_literal cdnf_lit then cdnf_var else - cdnf_var)
    | Cdnfp.And | Cdnfp.Or ->
        let n_args = Cdnfp.get_length f in
        let args =
          let helper i = from_boolformula_t (Cdnfp.get_boolformula f i) in
            Array.init n_args helper in
          if Cdnfp.get_type f = Cdnfp.And then And args 
          else Or args

let rec from_clauses res in_chan nvars nclauses =
  let rec helper res l =
    let _ = assert (abs l <= nvars) in
      if l = 0 then res
      else Scanf.fscanf in_chan " %d" (helper (l::res)) 
  in
    if nclauses = 0 then (nvars, And (Array.of_list res))
    else
      let ls = Scanf.fscanf in_chan " %d" (helper []) in
      let clause = Or (Array.of_list (List.rev_map (fun l -> Lit l) ls)) in
        from_clauses (clause::res) in_chan nvars (pred nclauses)

let from_dimacs in_chan =
  let rec helper () =
    let line = try input_line in_chan with End_of_file -> "" in
      if String.length line = 0 then (0, And [| |])
      else
        match line.[0] with
          | 'c' -> helper ()
          | 'p' -> 
              let _ = assert (String.sub line 0 5 = "p cnf") in
                Scanf.sscanf line "p cnf %d %d" (from_clauses [] in_chan)
          | _ -> assert false in
    helper ()

let rec print boolf =
  match boolf with
    | Lit l -> print_int l
    | Not arg ->
        (print_string "(!"; print arg; print_string ")")
    | And args | Or args ->
        let length = Array.length args in
          if length = 0 then
            print_string
              (match boolf with 
                 | And _ -> "T" | Or _ -> "F" | _ -> assert false)
          else
            let helper op i arg = 
              (print arg; if i < length - 1 then print_string op else ()) in
            let op = 
              match boolf with
                | And _ -> " & " | Or _ -> " | " | _ -> assert false in
              print_string "("; Array.iteri (helper op) args; print_string ")"
							

