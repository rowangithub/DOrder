let
#{size:int | 0 < size}
queen(size) =
  let queenArray = make_vect size 0 in
  let assign i j = queenArray..(i) <- j
#  withtype {i:nat | i < size} int(i) -> int -> unit in
  let rec dotsPrint n = if n = 0 then () else begin print_string "."; dotsPrint (n-1) end in
  let queenPrint () =
    let rec aux row = begin
      if eq_int row size then () else
      let n = queenArray..(row) in
        dotsPrint(n-1); print_string "Q"; dotsPrint(size - n); print_string "\n"; aux (row + 1)
      end
#    withtype {i:nat | i <= size } int(i) -> unit in
    aux(0); print_string "\n" in
  let test j =
    let qj = queenArray..(j) in
    let rec aux i =
      if lt_int i j then
        let qi = queenArray..(i) in
        if qi = qj then false else if abs(qj - qi) = j - i then false else aux (i+1)
      else true
#    withtype {i:nat | i < size} int(i) -> bool in aux 0
#  withtype {j:nat | j < size} int(j) -> bool in
  let rec loop row =
    let next = queenArray..(row) + 1 in
      if next > size then
        begin assign row 0; if eq_int row 0 then () else loop (row-1) end
      else
      begin assign row next;
        if test row then
          if eq_int (row+1) size then begin queenPrint(); loop(row) end else loop(row+1)
          else loop row
      end
#  withtype {row:nat | row < size} int(row) -> unit in loop(0)
#withtype int(size) -> unit
;;
