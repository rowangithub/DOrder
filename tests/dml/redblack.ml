(* DSOLVE -dontgenmlq -dontminemlq -bare *)

(* Adapted from an example of Dunfield, Xi, and Pfenning
 * http://type-refinements.info/stardust/thesis/examples/redblack-full.rml
 *)

type 'a dict =
    Empty
  | Black of 'a * 'a dict * 'a dict
  | Red of 'a * 'a dict * 'a dict
  | Purple of 'a * 'a dict * 'a dict

let color d = match d with
  | Empty -> 1
  | Black (a, b, c) -> 1
  | Red (a, b, c) -> 2
  | Purple (a, b, c) -> 3

let max x y =
  if x > y then x else y

let rec height d = match d with
  | Empty -> 0
  | Red (x, l, n) -> 
		if height l > height n
		then height l
		else height n
  | Black (x, l, n) -> 
		if height l > height n
		then height l + 1
		else height n + 1
  | Purple (x, l, n) -> 
		if height l > height n
		then height l
		else height n
		
let height1 t = match t with
	| Empty -> 0
	| Black (x, l, n) -> height l
	| Red (x, l, n) -> height l
	| Purple (x, l, n) -> height l

let height2 t = match t with
	| Empty -> 0
	| Black (x, l, n) -> height n
	| Red (x, l, n) -> height n
	| Purple (x, l, n) -> height n	

let color1 t = match t with
	| Empty -> 1
	| Black (x, l, n) -> color l
	| Red (x, l, n) -> color l
	| Purple (x, l, n) -> color l

let color2 t = match t with
	| Empty -> 1
	| Black (x, l, n) -> color n
	| Red (x, l, n) -> color n
	| Purple (x, l, n) -> color n	

let restore_right e lt n =
  match n with
  | Purple (re, rl, rr) ->
      begin match lt with
        | Red (le, ll, lr) ->
            begin match rl with
              | Red (a, b, c) ->
                  Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
              | Black (a, b, c)   -> (match rr with
							    |  Red (a, b, c) ->
							         Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
							    (*| Black (a, b, c)   -> assert (0 = 1); assert false
							    | Purple (a, b, c) -> assert (0 = 1); assert false
							    | Empty    -> assert (0 = 1); assert false *))
              (*| Purple _ -> assert (0 = 1); assert false*)
              | Empty    -> (match rr with
								    |  Red (a, b, c) ->
								         Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
								    (*| Black (a, b, c)   -> assert (0 = 1); assert false
								    | Purple (a, b, c) -> assert (0 = 1); assert false
								    | Empty    -> assert (0 = 1); assert false *))
            end
        | Black (a, b, c) ->
            begin match rl with
              | Red (rle, rll, rlr) ->
                  (* l is black, deep rotate *)
                  Black (rle, Red (e, lt, rll), Red (re, rlr, rr))
              | Black (a, b, c)   ->
                  (* l is black, shallow rotate *)
                  Black(re, Red(e, lt, rl), rr)
              | Empty ->
                  (* l is black, shallow rotate *)
                  Black(re, Red(e, lt, rl), rr)
              (*| Purple _ -> assert (0 = 1); assert false*)
            end
        (*| Purple _ -> assert (0 = 1); assert false*)
        | Empty ->
            begin match rl with
              | Red (rle, rll, rlr) ->
                  (* l is black, deep rotate *)
                  Black (rle, Red (e, lt, rll), Red (re, rlr, rr))
              | Black (a, b, c)   ->
                  (* l is black, shallow rotate *)
                  Black(re, Red(e, lt, rl), rr)
              | Empty ->
                  (* l is black, shallow rotate *)
                  Black(re, Red(e, lt, rl), rr)
              (*| Purple _ -> assert (0 = 1); assert false*)
            end
      end
  | Red (a, b, c)   -> Black (e, lt, n)
  | Black (a, b, c) -> Black (e, lt, n)
  | Empty   -> Black (e, lt, n)

let restore_left e l rt =
  match l with
  | Purple (le, ll, lr) ->
      begin match rt with
        | Red (re, rl, rr) ->
            begin match lr with
              | Red (a, b, c)    -> Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
              | Black (a, b, c)  -> (match ll with
								    | Red (a, b, c)    -> Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
								    (*| Black (a, b, c)  -> assert (0 = 1); assert false
								    | Purple (a, b, c) -> assert (0 = 1); assert false
								    | Empty    -> assert (0 = 1); assert false*))
              (*| Purple _ -> assert (0 = 1); assert false*)
              | Empty    -> (match ll with
								    | Red (a, b, c)    -> Red (e, Black (le, ll, lr), Black (re, rl, rr))     (* re-color *)
								    (*| Black (a, b, c)  -> assert (0 = 1); assert false
								    | Purple (a, b, c) -> assert (0 = 1); assert false
								    | Empty    -> assert (0 = 1); assert false*))
            end
        | Black (a, b, c) ->
            begin match lr with
              | Red (lre, lrl, lrr) ->
                  (* n is black, deep rotate *)
                  Black (lre, Red (le, ll, lrl), Red(e, lrr, rt))
              | Black (a, b, c)   ->
                  (* n is black, shallow rotate *)
                  Black (le, ll, Red (e, lr, rt))
              (*| Purple _ -> assert (0 = 1); assert false*)
              | Empty ->
                  (* n is black, shallow rotate *)
                  Black (le, ll, Red (e, lr, rt))
            end
        (*| Purple _ -> assert (0 = 1); assert false*)
        | Empty ->
            begin match lr with
              | Red (lre, lrl, lrr) ->
                  (* n is black, deep rotate *)
                  Black (lre, Red (le, ll, lrl), Red(e, lrr, rt))
              | Black (a, b, c)   ->
                  (* n is black, shallow rotate *)
                  Black (le, ll, Red (e, lr, rt))
              (*| Purple _ -> assert (0 = 1); assert false*)
              | Empty ->
                  (* n is black, shallow rotate *)
                  Black (le, ll, Red (e, lr, rt))
            end
      end
  | Red (a, b, c)   -> Black (e, l, rt)
  | Black (a, b, c) -> Black (e, l, rt)
  | Empty   -> Black (e, l, rt)

let rec ins_aux key d = 
	let t = match d with
  | Black(key1, left, right) ->
      if key = key1 then Black (key, left, right)
      else if key < key1 then restore_left key1 (ins_aux key left) right
      else restore_right key1 left (ins_aux key right)
  | Red (key1, left, right) ->
      if key = key1 then Red (key, left, right)
      else if key < key1 then
        let l' = ins_aux key left in
          begin match l' with
            | Red (a, b, c)     -> Purple (key1, l', right)
            | Black (a, b, c)   -> Red (key1, l', right)
            | Empty     -> Red (key1, l', right)
            (*| Purple (a, b, c) -> assert (0 = 1); assert false*)
          end
      else
        let n' = ins_aux key right in
          begin match n' with
            | Red (a, b, c)     -> Purple (key1, left, n')
            | Black (a, b, c)   -> Red (key1, left, n')
            | Empty     -> Red (key1, left, n')
            (*| Purple (a, b, c) -> assert (0 = 1); assert false*)
          end
  | Empty     -> Red (key, Empty, Empty)
  (*| Purple (a, b, c) -> assert (0 = 1); assert false*) in
	let _ = assert (height t = height d) in t

let rec insert d key =
  let dict = ins_aux key d in
  let t = match dict with
      | Purple (d, lt, rt) -> Black (d, lt, rt) (* re-color *)
      | Red (d, lt, rt)     -> Red (d, lt, rt)
      | Black (d, lt, rt)   -> Black (d, lt, rt)
      | Empty               -> Empty in
	let _ = assert (height t >= height d && height t <= height d + 1) in t

let harness key = 
	let t = Empty in
	insert t key