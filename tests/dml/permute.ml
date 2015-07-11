let rec conslist x xss = 
	match xss with
		| [] -> []
		| xs :: xss -> (x :: xs) :: (conslist x xss)
(*(withtype {n:nat,p:nat} <p> =>
         'a * ('a list(n)) list(p) -> ('a list(n+1)) list(p)*)

let rec perm xs = 
	match xs with
		| [] -> [[]]
		| x :: xs -> perm_aux x [] xs (conslist x (perm xs))
(*withtype {n:nat} <n,0> => 'a list(n) -> ('a list(n)) list*)

and perm_aux x ys zs res = 
	match zs with
		| [] -> res
		| z::zs -> perm_aux x (z :: ys) zs
            (conslist z (perm (ys @ (x :: zs))) @ res)  
(*withtype {p:nat,q:nat,n:pos | p+q=n-1} <n-1,q+1> =>
         'a * 'a list(p) * 'a list(q) * ('a list(n)) list ->
         ('a list(n)) list*)
				
let main () = 
	let l = [30; 20; 50; 10; 40] in
	perm l
		
let _ = main ()		