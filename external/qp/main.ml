module Misc = QpMisc
module A = QpAst
module P = A.Predicate
module E = A.Expression

let filename_to_ps f = 
  let _ = QpErrormsg.startFile f in
  let inchan = open_in f in
  QpParse.predlist QpLex.token (Lexing.from_channel inchan)

let main () = 
  Printf.printf "QProver 0.1. Copyright 2008, Regents of the University of California. " ;
  Printf.printf "All Rights Reserved.\n";
  Printf.printf "$ %s \n" (String.concat " " (Array.to_list Sys.argv));
  let n = ref 1 in
  if (Array.length Sys.argv - !n >= 1) then 
    let ps = filename_to_ps (Sys.argv.(!n)) in 
    match ps with [] -> exit 0 | p::qs -> 
      let ch = QProver.check_imp p in
      let outs = String.concat "," (List.map (fun q -> string_of_bool (ch q)) qs) in
      let _ = Printf.printf "result = %s \n" outs in
      exit 0

let _ = main ()

  (* {{{ let _ = 
    while !n < Array.length Sys.argv && String.length Sys.argv.(!n) > 0 && Sys.argv.(!n).[0] = '-' do
      (     if Sys.argv.(!n) = "-pa" then dom := PA 
       else if Sys.argv.(!n) = "-cpa" then dom := CartesianPA
       else if Sys.argv.(!n) = "-w" then w := true
       else if Sys.argv.(!n) = "-mc" then dom := MC
       else if Sys.argv.(!n) = "-size" then (size := int_of_string (Sys.argv.(!n+1)); n:=!n+1)
       else if Sys.argv.(!n) = "-debug" then Message.set_level Message.Debug);
      n := !n + 1
    done in }}} *)
(* {{{
let t1 = 
  (A.And [A.Equality (A.Variable "x", A.Variable "y")],
  [A.Equality (A.Variable "y", A.Variable "x")],
  [true])

let t2 = 
  (A.And [A.Equality (A.Variable "x", A.Variable "y")],
  [A.Not(A.Equality (A.Variable "y", A.Variable "x"))],
  [false])

let t3 = 
  (A.And [A.Equality (A.Variable "x", A.Variable "y"); 
          A.Equality (A.Variable "y", A.Variable "z")],
  [A.Equality (A.Variable "z", A.Variable "x");
  A.Equality (A.Application ("f",[A.Variable "x"]), A.Application ("f",[A.Variable "z"]))],
  [true;true])

let t4 = 
  (A.And [A.Equality (A.Variable "x", A.Variable "y"); 
          A.Equality (A.Variable "y", A.Variable "z");
          A.Not (A.Equality (A.Application ("f",[A.Variable "x"]), A.Application ("f",[A.Variable "z"])))],
  [A.Leq (A.Variable "a", A.Variable "b")],
  [true])

let tests = 
  [t1;t2;t3;t4]

let do_test i (p,qs,r) = 
  let res = if r = QProver.check_imps p qs then "OK" else "FAIL" in
  Printf.printf "test %d (%s) : %s => %s \n" i res (P.toString p) (P.toString (A.And qs)) 
}}} *)


