open Typedtree
open Parsetree

let type_qualifier (name, pat) =
  let (valu, _, pred) = pat.pqual_pat_desc in
  let valu = Ident.create valu in
  let pred = Qualdecl.transl_patpred_single true (Path.Pident valu) Env.empty pred in
   Tstr_qualifier (Ident.create name, (valu, pred))
