let
#{size:int | size > 0}
play size =
  let leftPost = make_vect size 0
  and middlePost = make_vect size 0
  and rightPost = make_vect size 0 in

  let initialize post =
    for i = 0 to size - 1 do
      post..(i) <- i+1
    done
#  withtype int vect(size) -> unit in

  let showpiece n =
    for i = 1 to n do print_string "O" done;
    for i = n + 1 to size do print_string " " done in

  let showposts () =
    for i = 0 to size - 1 do
      showpiece leftPost..(i);
      print_string "  ";
      showpiece middlePost..(i);
      print_string "  ";
      showpiece rightPost..(i);
      print_newline ()
    done;
    print_newline ()
#  withtype unit -> unit in

  let _ = initialize(leftPost) in
  let rec move (n, source, s, post, p, post', p') =
    if eq_int n 1 then
      begin post..(p - 1) <- source..(s); source..(s) <- 0; showposts() end
    else begin
      move(n-1, source, s, post', p', post, p);
      post..(p - 1) <- source..(s + n - 1);
      source..(s + n - 1) <- 0;
      showposts ();
      move(n-1, post', p'-n+1, post, p - 1, source, s + n)
    end
#  withtype {n:nat}{s:nat}{p:nat}{p':nat |
#            p <= size /\ p' <= size /\ s + p + p' = size + size /\ 0 < n /\
#            s + n <= size /\ n <= p /\ n <= p' }
#           int(n) * int vect(size) * int(s) * int vect(size) * int(p) * int vect(size) * int(p') -> unit in
  showposts();
  move(size, leftPost, 0, rightPost, size, middlePost, size)
#withtype int(size) -> unit
;;
