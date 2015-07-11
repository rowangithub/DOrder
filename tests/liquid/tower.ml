let print_newline _1_none = () 
let print_string _2_none = () 

let
play sz =
  let leftPost = Array.make sz 0 in
  let middlePost = Array.make sz 0 in
  let rightPost = Array.make sz 0 in
	let sz_minus = sz - 1 in

  let initialize _1_post =
		let _1_i = ref 0 in
		let rec initialize_rec _3_none = 
			let deref_1_i = !_1_i in
			let deref_1_i_plus = deref_1_i + 1 in
			if deref_1_i < sz_minus then (Array.set _1_post deref_1_i deref_1_i_plus; _1_i := deref_1_i_plus; initialize_rec ()) 
											 else ()
		in initialize_rec ()
  in

  let showpiece _1_n =
		let _1_n_plus = _1_n + 1 in
		let _2_i = ref 1 in
		let rec showpiece_r_rec _4_none =
			let deref_2_i = !_2_i in
			let deref_2_i_plus = deref_2_i + 1 in
			if deref_2_i_plus < _1_n then (print_string (); _2_i := deref_2_i_plus; showpiece_r_rec ())
										 else ()
		in
		let j = ref _1_n_plus in
		let rec showpiece_r2_rec _5_none =		
			let deref_j = !j in
			let deref_j_plus = deref_j + 1 in
			if deref_j < sz then (print_string (); j := deref_j_plus; showpiece_r2_rec ())
										 else ()
		in (showpiece_r_rec (); showpiece_r2_rec ())
	in

  let showposts _6_none =
		let _3_i = ref 0 in
		let rec showposts_rec _6_none =
			let deref_3_i = !_3_i in
			let deref_3_i_plus = deref_3_i + 1 in
			if deref_3_i < sz_minus then
      (showpiece (Array.get leftPost deref_3_i);
			print_string ();
      showpiece (Array.get middlePost deref_3_i);
			print_string ();
      showpiece (Array.get rightPost deref_3_i);
      print_newline ();
			_3_i := deref_3_i_plus;
			showposts_rec ())
			else ()
		in (showposts_rec (); print_newline ())
  in

  let _7_none = initialize leftPost in
  let rec move n source s post p post' p' =
		let n_minus = n - 1 in
		(*let n_plus = n + 1 in*)
		let p_minus = p - 1 in
		let s_plus_n_minus = s + n_minus in
		let pp_minus_n_plus = p' - n_minus in
		let s_plus_n = s + n in
    if n = 1 then
      let gss = Array.get source s in
      begin Array.set post p_minus gss; Array.set source s 0; showposts() end
    else begin
      (move n_minus source s post' p' post p;
      let gs = Array.get source s_plus_n_minus in
      Array.set post p_minus gs;
      Array.set source s_plus_n_minus 0;
      showposts ();
      move n_minus post' pp_minus_n_plus post p_minus source s_plus_n)
    end
	in
  (showposts ();
  move sz leftPost 0 rightPost sz middlePost sz)

let driver = play 10    

(*
  let rec move (n, source, s, post, p, post', p') =
    if n = 1 then
      begin post[p - 1] <-  source[s]; source[s] <- 0 end
    else begin
      move(n-1, source, s, post', p', post, p);
      post[p - 1] <- source[s + n - 1];
      source[s + n - 1] <- 0;
      move(n-1, post', p'-n+1, post, p - 1, source, s + n)
    end
  move(size, leftPost, 0, rightPost, size, middlePost, size)
*)


(*
let{Array.length:int | Array.length > 0}
play Array.length =
  let leftPost = Array.make_vect Array.length 0
  and middlePost = Array.make_vect Array.length 0
  and rightPost = Array.make_vect Array.length 0 in

  let initialize post =
    for i = 0 to Array.length - 1 do
      post..(i) <- i+1
    done
  withtype int vect(Array.length) -> unit in

  let showpiece n =
    for i = 1 to n do print_string "O" done;
    for i = n + 1 to Array.length do print_string " " done in

  let showposts () =
    for i = 0 to Array.length - 1 do
      showpiece leftPost..(i);
      print_string "  ";
      showpiece middlePost..(i);
      print_string "  ";
      showpiece rightPost..(i);
      print_newline ()
    done;
    print_newline ()
  withtype unit -> unit in

  let _ = initialize(leftPost) in
  let rec move (n, source, s, post, p, post', p') =
    if n = 1 then
      begin post[p - 1] <-  source[s]; source[s] <- 0 end
    else begin
      move(n-1, source, s, post', p', post, p);
      post[p - 1] <- source[s + n - 1];
      source[s + n - 1] <- 0;
      move(n-1, post', p'-n+1, post, p - 1, source, s + n)
    end
  move(size, leftPost, 0, rightPost, size, middlePost, size)
;; *)
