let sub2 data i j = 
  Bigarray.Array2.get data i j 

let update2 data i j x = 
  Bigarray.Array2.set data i j x

let fillar arr2 fill =
  let d1 = Bigarray.Array2.dim1 arr2 in
  let d2 = Bigarray.Array2.dim2 arr2 in
  let rec loop i =
    let i' = i + 1 in 
      if i < d1 
      then
        let rec loopi j =
          let j' = j + 1 in
           if j < d2 then
             let elt = fill () in
             (Bigarray.Array2.set arr2 i j elt; loopi j')
           else
             ()
        in loopi 0
      else
          loop i'
  in loop 0

let matmul a b =
  let p = Bigarray.Array2.dim1 a in
  let q = Bigarray.Array2.dim2 a in
  let r = Bigarray.Array2.dim2 b in

  let cdata = Bigarray.Array2.create Bigarray.int Bigarray.c_layout p r in
  let zero _none = 0 in
  let _none = fillar cdata zero in

  let rec loop1 i =
    if i < p then
      let rec loop2 j =
	if j < r then
	  let rec loop3 k sum =
	    if k < q then
              let k' = k + 1 in
              let _none = (fun s -> s) i in
              let saik = sub2 a i k in
              let _none = (fun s -> s) k in
              let sbkj = sub2 b k j in
              let sum_p = sum + saik in
              let sum_p = sum_p + sbkj in
		loop3 k' sum_p 
	    else sum
          in let l3 = loop3 0 0 in
	  let _none = update2 cdata i j l3  in
          let j' = j + 1 in
	    loop2 j'
	else () in
      let _none = loop2 0 in
      let i' = i + 1 in
	loop1 i'
    else ()
  in loop1 0; cdata


let driver = 
  let _none = Random.self_init () in
  let p = Random.int 10 in
  let p = p + 1 in
  let q = Random.int 10 in
  let q = q + 1 in
  let r = Random.int 10 in
  let r = r + 1 in
  let rand _none = Random.int 100 in
  let av = Bigarray.Array2.create Bigarray.int Bigarray.c_layout p q in
  let bv = Bigarray.Array2.create Bigarray.int Bigarray.c_layout q r in 
    fillar av rand; fillar bv rand; matmul av bv;;


(*withtype {p:nat,q:nat,r:nat} <> =>
         float matrix(p,q) * float matrix(q,r) -> float matrix(p,r)*)


(*fun matmul (a, b) =
    let
	val Matrix (p, q, adata) = a
	val Matrix (_, r, bdata) = b
	val cdata = alloc (p, alloc (r, 0.0))

	fun init (i) =
            if i < p then
	       let
		   val _ = update (cdata, i, alloc (r, 0.0))
	       in
		   init (i+1)
	       end
	    else ()
	withtype {i:pos} <max(p-i, 0)> => int(i) -> unit

	val _ = init (1)

	fun loop1 (i) =
	    if i < p then
		let
		    fun loop2 (j) =
			if j < r then
			    let
				fun loop3 (k, sum) =
				    if k < q then
					loop3 (k+1, sum +. sub2 (adata, i, k) *. sub2 (bdata, k, j))
				    else sum
				withtype {k:nat} <max(q-k,0)> => int(k) * float -> float
				val _ = update2 (cdata, i, j, loop3 (0, 0.0))
			    in
				loop2 (j+1)
			    end
			else ()
		    withtype {j:nat} <max(r-j,0)> => int(j) -> unit
		    val _ = loop2 (0)
		in
		    loop1 (i+1)
		end
	    else ()
        withtype {i:nat} <max(p-i,0)> => int(i) -> unit
	val _ = loop1 (0)
    in
	Matrix (p, r, cdata)
    end
withtype {p:nat,q:nat,r:nat} <> =>
         float matrix(p,q) * float matrix(q,r) -> float matrix(p,r)*)
