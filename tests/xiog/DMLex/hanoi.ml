fun repeat_pr (s, n) =
  if n > 0 then
    let val _ = print_string (s) in repeat_pr (s, n-1) end
  else ()
withtype {n:int} <max(n,0)> => string * int(n) -> unit

fun print_disks (size, n) =
  let
      val _ = repeat_pr ("O", n)
  in
      repeat_pr (" ", size - n)
  end
withtype {size:int, n:int} <> => int(size) * int(n) -> unit

fun print_posts (size, posts) =
  let
      fun aux (i, size, posts) =
        if i < size then
          let
              val _ = print_disks (size, sub (sub (posts, 0), i))
              val _ = print_string ("\t")
              val _ = print_disks (size, sub (sub (posts, 1), i))
              val _ = print_string ("\t")
              val _ = print_disks (size, sub (sub (posts, 2), i))
              val _ = print_string ("\n")
          in
              aux (i+1, size, posts)
          end
        else ()
      withtype {i:nat, size:int | i <= size} <size - i> =>
               int(i) * int(size) * (int array(size)) array(3) -> unit
   in
      aux (0, size, posts)
   end
withtype {size:nat} <> => int(size) * (int array(size)) array(3) -> unit

fun move (size, posts, n, sp, s, dp1, d1, dp2, d2) =
  if (n = 1) then
    let
        val _ = update (sub (posts, dp1), d1 - 1, sub (sub (posts, sp), s))
        val _ = update (sub (posts, sp), s, 0)
        val _ = print_posts (size, posts)
    in
       print_newline ()
    end
  else
    let
        val _ = move (size, posts, n-1, sp, s, dp2, d2, dp1, d1)
        val _ = update (sub (posts, dp1), d1 - 1, sub (sub (posts, sp), s+n-1))
        val _ = update (sub (posts, sp), s+n-1, 0)
        val _ = print_posts (size, posts)
        val _ = print_newline ()
    in
       move (size, posts, n-1, dp2, d2-n+1, dp1, d1-1, sp, s+n)
    end
withtype {size:int, n:pos, s:nat, d1:int, d2:int |
          d1 <= size, d2 <= size, s+d1+d2=size+size,
          s+n <= size, n <= d1, n <= d2} <n> =>
         int(size) * (int array(size)) array(3) * int(n) *
         int[0,2] * int(s) * int[0,2] * int(d1) * int[0,2] * int(d2) -> unit

fun init (i, size, posts) =
  if i < size then
    let
        val _ = update (sub (posts, 0), i, i+1)
    in
        init (i+1, size, posts)
    end
  else ()
withtype {i:int, size:int | 0 <= i <= size} <size - i> =>
         int(i) * int(size) * (int array(size)) array(3) -> unit

fun play (size) =
  let
      val posts = alloc (3, alloc (size, 0))
      val _ = update(posts, 1, alloc (size, 0))
      val _ = update(posts, 2, alloc (size, 0))
      val _ = init (0, size, posts)
      val _ = print_posts (size, posts)
      val _ = print_newline ()
  in
     move (size, posts, size, 0, 0, 1, size, 2, size)
  end
withtype {size:pos} <> => int(size) -> unit

fun main () = play (8)
withtype <> => unit -> unit
