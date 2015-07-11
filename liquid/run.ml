open Unix
open Format

let target = "./target"

exception RUNFAIL
 
(** Call this method to run a ocaml file *)
let execute () = 
	let code = fork () in
  match code with
    | 0 -> (
			try execvp target [|target(*; "-l"*)|]
      with _ -> 
				fprintf err_formatter "@[%s@]@." "error while execv execute\n"; raise RUNFAIL)
    | -1 -> 
			(fprintf err_formatter "@[%s@]@." "error accured on execute fork\n"; raise RUNFAIL)
    | _ -> (ignore (wait ()); printf "%s" "program excuted ...\n")

let compile filename = 
	let code = fork () in
  match code with
    | 0 -> (
			try execvp "ocamlc" 
					[|"ocamlc"; "-o"; target; filename|]
      with _ -> 
				fprintf err_formatter "@[%s@]@." "error while compile execv\n"; raise RUNFAIL)
    | -1 -> 
			(fprintf err_formatter "@[%s@]@." "error accured on compile fork\n"; raise RUNFAIL)
    | _ -> (
			let (_, status) = wait () in 
			match status with
				| WEXITED code -> 
					if (code = 0) then fprintf err_formatter "@[%s@]@." "program compiled...\n"
					else (assert false)
				| _ -> assert false)

let sample filename = 
	(compile filename; execute ())

(** Assumption: good.mat and bad.mat are prepared
 	* Return an vector representing solution
	*)	
let learn () = 
	let code = fork () in
  match code with
    | 0 -> (
			try execvp "matlab" 
					[|"matlab"; "-nosplash"; "-nodesktop"; "-r"; "run"|]
      with _ -> 
				fprintf err_formatter "@[%s@]@." "error while execv matlab\n"; raise RUNFAIL)
    | -1 -> 
			(fprintf err_formatter "@[%s@]@." "error accured on matlab fork\n"; raise RUNFAIL)
    | _ -> (
			let (_, status) = wait () in 
			match status with
				| WEXITED code -> 
					if (code = 0) then fprintf err_formatter "@[%s@]@." "matlab learned...\n"
					else (assert false)
				| _ -> assert false)	