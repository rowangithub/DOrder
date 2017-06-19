let rec loopa i j n =
	if ( j <= n ) then
    (*//    tmpl("le(i,j,k,n)");
    // i = 0;
    // k = 0;*)
    if ( i >= 0) then
    (*
    while( i < n ) {
      //      tmpl("le(i,j,k,n)");
      assert( k>=i);
      i++;
      k++;
    }
    *)
    	loopa i (j+1) n
		else loopa i j n
  else assert( i>= 0)

let main j n =
  let i = 0 in
  let k = 0 in

  if ( j<=n ) then
		loopa i j n
	else ()
	(*
  j = 0;
  while( j < n ) {
    //    tmpl("le(i,j,k,n)");
    assert(k>0);
    j++;
    k--;
  }
  *)