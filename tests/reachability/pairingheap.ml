type 'a heap = 
	| E 
	| T of 'a * ('a heap) list

(*  let empty = E
  let is_empty h = h = E *)

let merge h1 h2 = 
	match h1, h2 with
    | h1, E -> h1
    | E, h2 -> h2
    | T (x, hs1), T (y, hs2) ->
        if x <= y then T (x, h2 :: hs1)
        else T (y, h1 :: hs2)

let insert x h =
	let em = ([]: ('a heap) list) in 
	merge (T (x, em)) h
let harness1 () = insert 0 E


let rec merge_pairs heap =
	match heap with 
    | [] -> E
		| hp1::heap' -> (
			match heap' with
				| [] -> hp1
				| hp2::hs -> merge (merge hp1 hp2) (merge_pairs hs)
			)
    (*| [h] -> h
    | h1 :: h2 :: hs -> merge (merge h1 h2) (merge_pairs hs) *)

(*let find_min h =
	match h with 
  (*| E -> raise Empty*)
  | T (x, hs) -> x *)

let delete_min h =
	match h with
    (*| E -> raise Empty *)
    | T (x, hs) -> (x, merge_pairs hs)
let harness2 () = delete_min E 