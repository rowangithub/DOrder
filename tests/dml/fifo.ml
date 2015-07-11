(* From SML Library *)
type 'a fifo = Q of 'a list * 'a list		

let reverse l =
	let rec aux xs ys = 
		match xs with
			| [] -> ys
			| x::zs -> aux zs (x::ys) in
    aux l []

let rec qlength q = match q with
	| Q (fs, rs) -> List.length fs + List.length rs

let enqueue q x = match q with
	| Q (fs, rs) -> Q (fs, x::rs)
(*withtype {l:nat} <> => 'a fifo(l) * 'a -> 'a fifo(l+1)*)

let dequeue q = match q with
	| Q (fs, rs) -> (match fs with
		| f :: fs ->  (Q (fs, rs), f)
		| [] -> (match reverse rs with
			| f :: fs -> (Q (fs, []), f))
	)
(*withtype {l:pos} <> => 'a fifo(l) -> 'a fifo(l-1) * 'a*)

let delete q p = 
	let rec doRear xs = match xs with
		(*| [] -> assert false*)
		| (r::rs) -> if p r = 1 then rs else r :: (doRear rs) in
	(*withtype {n:nat} <n> => 'a list(n) -> [n':nat | n'+1=n] 'a list(n')*)
	let rec doFront fs rs = match fs with
		| [] -> 
			let fs = doRear (reverse rs) in 
			(fs, [])
		| f :: fs ->
			if p f = 1 then (fs, rs) 
			else
				let (fs, rs) = doFront fs rs in
				(f::fs, rs) in
		(*withtype {m:nat,n:nat} <m> =>
		 'a list(m) * 'a list(n) ->
		     [m':nat,n':nat | m'+n'+1=m+n] ('a list(m') * 'a list(n'))*)
	match q with
		| Q (fs, rs) -> 		
			let (fs, rs) = doFront fs rs in
			Q (fs, rs)	 
(*withtype {l:pos} <> => 'a fifo(l) * ('a -> bool) -> 'a fifo(l-1)*)

let head q = match q with
	| Q (fs, rs) -> 
		(match fs with 
			| f :: fs -> f
			| [] -> (match rs with
				| r'::rs' -> match (reverse rs) with
					| r::rs -> r
				)
		)
(*withtype {l:pos} <> => 'a fifo(l) -> 'a*)
	
let contents q = match q with
	| Q (fs, rs) -> fs @ (reverse rs)
(*withtype {l:nat} <> => 'a fifo(l) -> 'a list(l)*)

let main q = 
	let q1 = enqueue q 0 in
	let (q2,_) = dequeue q1 in
	let q3 = delete q2 (fun x -> x) in 
	let q4 = contents q3 in
	assert (qlength q + 1 = qlength q1)
	
let _ = main (Q ([20;3;40], [5;4;30;2;1]))