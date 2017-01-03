(*
 * Copyright Â© 2008 The Regents of the University of California. All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

open Config
open Format
open Liqerrors
open Misc
open Types
open Clflags


module F = Frame

let usage = "Usage: liquid <options> [source-file]\noptions are:"

let filename = ref ""

let file_argument fname = filename := fname

let init_path () =
  let dirs =
    if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
    else if !Clflags.use_vmthreads then "+vmthreads" :: !Clflags.include_dirs
    else !Clflags.include_dirs in
  let exp_dirs =
    List.map (expand_directory Config.standard_library) dirs in
  load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
  Env.reset_cache ()

let initial_env () =
  Ident.reinit();
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial;
  with Not_found ->
    failwith "cannot open pervasives.cmi"

let initial_fenv env = 
	Lightenv.addn (Builtins.frames env) Lightenv.empty

let (++) x f = f x

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let type_implementation initial_env ast =
  Typecore.reset_delayed_checks ();
  let str = Typemod.type_structure initial_env ast Location.none in
    Typecore.force_delayed_checks ();
    str

let analyze ppf sourcefile (str, tystr, env, fenv, ifenv) =
  (*try*)
		Qualifymod.qualify_implementation sourcefile fenv ifenv env [] str tystr
	(*with _ -> (
		if (!Clflags.preservation) then
			Format.fprintf Format.std_formatter 
				"Sorry! Preservation properties is not fully supported for %s@." sourcefile
		else
			Format.fprintf Format.std_formatter
				"Inferring invariants for %s encouters internal bug. Please contact@."
				sourcefile	
	)*)

let load_qualfile ppf qualfile =
  let qs = Pparse.file ppf qualfile Parse.qualifiers ast_impl_magic_number in
    List.map Qualmod.type_qualifier qs

let load_mlqfile ppf env ifacefile =
  let (preds, vals) = if Sys.file_exists ifacefile then Pparse.file ppf ifacefile Parse.liquid_interface ast_impl_magic_number else ([], []) in
    List.map (fun (s, pf) -> (s, F.translate_pframe env preds pf)) vals 

let lookup_path s env =
  fst (Env.lookup_value (Longident.parse s) env)

let load_mlq_in_env env fenv ifenv =
  let load_frame fenv (s, pf) =
    try
      let p = lookup_path s env in
      (*let ff = Frame.fresh_without_vars env ((Env.find_value p env).val_type)
       * in*)
      let _ = if String.contains s '.' then failwith (Printf.sprintf "mlq: val %s has invalid name" s) in    
      (*let _ = if not(F.same_shape true ff pf) then
                failwith (sprintf "mlq: val %s has shape which differs from usage" s) in*)
        Lightenv.add p pf fenv 
    with Not_found -> failwith (Printf.sprintf "mlq: val %s does not correspond to program value" s) in
  List.fold_left load_frame fenv ifenv

let load_builtins ppf env fenv =
  let b = match !builtins_file with 
          | Some b -> if not(Sys.file_exists b) then failwith (sprintf "builtins: file %s does not exist" b) else b
          | None -> "" in
  let fenv = 
    try
      let kvl = load_mlqfile ppf env b in
      let f = (fun (k, v) -> (lookup_path k env, F.label_like v v)) in
      let kvl = List.map f kvl in
      Lightenv.addn kvl fenv
    with Not_found -> failwith (Printf.sprintf "builtins: val %s does not correspond to library value" b) in
  (env, fenv)
    
let load_sourcefile ppf sourcefile =
  init_path ();
  let env = initial_env () in
  let fenv = initial_fenv env in
  let imp_str = Pparse.file ppf sourcefile Parse.implementation ast_impl_magic_number in 
	(*let sourcetext = Pprintast.string_of_structure str in
	let _ = print_string sourcetext in
	let _ = Run.sample "./tests/bsearch.ml" in*)
  let str = if !Clflags.no_anormal then imp_str else Normalize.normalize_structure imp_str in
  let str = print_if ppf Clflags.dump_parsetree Printast.implementation str in
  let (tystr, _, env) = type_implementation env str in
    (str, tystr, env, fenv)

let process_sourcefile fname =
  let bname = Misc.chop_extension_if_any fname in
  let (qname, iname) = (bname ^ ".quals", bname ^ ".mlq") in
  try
   let (str, tystr, env, fenv) = load_sourcefile std_formatter !filename in
   if !dump_qualifs
   then
     Qdump.dump_default_qualifiers (tystr, env, fenv) qname
   else
    let quals = load_qualfile std_formatter qname in
     let (env, fenv) = load_builtins std_formatter env fenv in 
     let ifenv = load_mlqfile std_formatter env iname in
     let ifenv = load_mlq_in_env env Lightenv.empty ifenv in
     let source = (str, List.rev_append quals tystr, env, fenv, ifenv) in
     analyze std_formatter !filename source
  with x -> (report_error std_formatter x; exit 1)

let main () =
  Arg.parse [
     "-I", Arg.String(fun dir ->
       let dir = expand_directory Config.standard_library dir in
       include_dirs := dir :: !include_dirs),
           "<dir>  Add <dir> to the list of include directories";
     "-init", Arg.String (fun s -> init_file := Some s),
           "<file>  Load <file> instead of default init file";
     "-labels", Arg.Clear classic, " Labels commute (default)";
     "-noassert", Arg.Set noassert, " Do not compile assertion checks";
     "-nolabels", Arg.Set classic, " Ignore labels and do not commute";
     "-noprompt", Arg.Set noprompt, " Suppress all prompts";
     "-nostdlib", Arg.Set no_std_include,
           " do not add default directory to the list of include directories";
     "-principal", Arg.Set principal, " Check principality of type inference";
     "-rectypes", Arg.Set recursive_types, " Allow arbitrary recursive types";
     "-unsafe", Arg.Set fast, " No bound checking on array and string access";
     "-w", Arg.String (Warnings.parse_options false),
           "<flags>  Enable or disable warnings according to <flags>:\n\
       \032    A/a enable/disable all warnings\n\
       \032    C/c enable/disable suspicious comment\n\
       \032    D/d enable/disable deprecated features\n\
       \032    E/e enable/disable fragile match\n\
       \032    F/f enable/disable partially applied function\n\
       \032    L/l enable/disable labels omitted in application\n\
       \032    M/m enable/disable overriden method\n\
       \032    P/p enable/disable partial match\n\
       \032    S/s enable/disable non-unit statement\n\
       \032    U/u enable/disable unused match case\n\
       \032    V/v enable/disable hidden instance variable\n\
       \032    Y/y enable/disable suspicious unused variables\n\
       \032    Z/z enable/disable all other unused variables\n\
       \032    X/x enable/disable all other warnings\n\
       \032    default setting is \"Aelz\"";
     "-warn-error" , Arg.String (Warnings.parse_options true),
       "<flags>  Treat the warnings of <flags> as errors, if they are enabled.\n\
         \032    (see option -w for the list of flags)\n\
         \032    default setting is a (all warnings are non-fatal)";

     "-dparsetree", Arg.Set dump_parsetree, " (undocumented)";
     "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
     "-dlambda", Arg.Set dump_lambda, " (undocumented)";
     "-dinstr", Arg.Set dump_instr, " (undocumented)";
     "-dconstrs", Arg.Set dump_constraints, "print out constraints";
     "-dqexprs", Arg.Set dump_qexprs, "print out all subexpressions with their qualified types";
     "-dqualifs", Arg.String (fun s -> dump_qualifs := true; Qdump.patf := s), "<file> dump qualifiers for patterns in <file>";
     "-dqueries", Arg.Set dump_queries, "print out all theorem prover queries and their results";
     "-dframes", Arg.Set dump_frames, "place frames in an annotation file";
     "-lqueries", Arg.Set log_queries, "log queries to [prover].log";
     "-cqueries", Arg.Set check_queries, "use a backup prover to check all queries";
     "-bquals", Arg.Set brief_quals, "print out the number of refinements for a type instead of their names";
     "-no-simple", Arg.Set no_simple, "do not propagate in simple constraints";
     "-no-simple-subs", Arg.Set no_simple_subs, "do not propagate sets when substitutions are present";
     "-verify-simple", Arg.Set verify_simple, "verify simple constraint propagation against theorem prover result";
     "-use-list", Arg.Set use_list, "use worklist instead of heap in solver";
     "-bprover", Arg.Set always_use_backup_prover, "always use backup prover";
     "-qprover", Arg.Set use_qprover , "use Qprover";
     "-qpdump", Arg.Set qpdump, "dump Qprover queries";
     "-lqualifs", Arg.Set less_qualifs, "only collect formal parameter identifiers";
     "-no-anormal", Arg.Set no_anormal, "don't rewrite the AST for a-normality";
     "-ksimpl", Arg.Set kill_simplify, "kill simplify after a large number of queries to reduce memory usage";
     "-cacheq", Arg.Set cache_queries, "cache theorem prover queries";
     "-psimple", Arg.Set psimple, "prioritize simple constraints";
     "-simpguard", Arg.Set simpguard, "simplify guard (remove iff)";
		 "-effect", Arg.Clear no_effect, "consider side effects";
		 "-inv", Arg.Set gen_inv, "generate array invariants";
		 "-preservation", Arg.Set preservation, "generate array preservation invariants";
		 "-hoflag", Arg.Set hoflag, "higher-order analysis";
		 "-no_hoflag", Arg.Set no_hoflag, "do not analyze higher-order functions";
		 "-reachability", Arg.Set reachability, "reachability analysis";
		 "-dump_specs", Arg.Set dump_specs, "dump specifications into files";
     "-v", Arg.Int (fun c -> Common.verbose_level := c), 
              "<level> Set degree of analyzer verbosity:\n\
               \032    0      No output\n\
               \032    1      +Verbose errors\n\
               \032    [2]    +Verbose stats, timing\n\
               \032    3      +Print normalized source\n\
               \032    11     +Verbose solver\n\
               \032    13     +Dump constraint graph\n\
               \032    64     +Drowning in output";
     "-collect", Arg.Int (fun c -> Qualgen.col_lev := c), "[1] number of lambdas to collect identifiers under";
     "-use-builtins", Arg.String (fun s -> builtins_file := Some s), "[None] location of extra builtins"
  ] file_argument usage;
  process_sourcefile !filename

let _ = 
  Printf.printf "MSolve 1.0: © Copyright 2016 Purdue Univerisity, All rights reserved \n";
	main (); exit 0
