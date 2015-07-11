(* TODO: add all the printing back in *)

let abs x = if x < 0 then let y = 0 - x in y else x 

let queen _1_size =
begin
  let queenArray = Array.make _1_size 0 in
  let assign _1_i _1_j = Array.set queenArray _1_i _1_j in
  let rec dotsPrint _2_n = 
			begin
			if _2_n = 0 then () else 
				let _2_n_minus = _2_n - 1 in dotsPrint _2_n_minus
			end
	in  
  let queenPrint _none =
	begin
    let rec _1_aux _1_row = 
		begin
      if _1_row = _1_size then () else
      let _1_n = Array.get queenArray _1_row in 
			let _1_n_minus = _1_n - 1 in 
			let _1_size_minus_1_n = _1_size - _1_n in 
			let _1_row_plus = _1_row + 1 in
			dotsPrint _1_n_minus; dotsPrint _1_size_minus_1_n; _1_aux _1_row_plus 
    end
    in _1_aux 0
	end 
  in
  let test _2_j =
    let q2j = Array.get queenArray _2_j  in
    let rec _2_aux _2_i =
		begin
      if _2_i < _2_j then
        let q2i = Array.get queenArray _2_i in
				let qdiff = q2j - q2i in
				let absqdiff = abs qdiff in
        let j_minus_i = _2_j - _2_i in
        if q2i = q2j then false else if absqdiff = j_minus_i then false else 
					let _2_i_plus = _2_i + 1 in _2_aux _2_i_plus
      else true
		end
    in _2_aux 0  
  in
  let rec loop _2_row =
		let _2_row_minus = _2_row - 1 in
		let _2_row_plus = _2_row + 1 in
		let _get_queenArray_2_row = Array.get queenArray _2_row in
    let next = _get_queenArray_2_row + 1 in
      if _1_size < next then
        begin assign _2_row 0; if _2_row = 0 then () else loop _2_row_minus end
      else
      begin 
				assign _2_row next;
        if test _2_row then
				begin
          if _2_row_plus = _1_size then begin queenPrint (); loop _2_row end else loop _2_row_plus
				end
        else loop _2_row
      end  
	in loop 0
end 

let driver = 
  let _none = Random.init 555 in
  let sz = Random.int 10 in
  let sz_plus = sz + 1 in
    queen sz_plus 




(*let{size:int | 0 < size}
queen(size) =
  let queenArray = Array.make_vect Array.length 0 in
  let assign i j = queenArray..(i) <- j
  withtype {i:nat | i < size} int(i) -> int -> unit in
  let rec dotsPrint n = if n = 0 then () else begin print_string "."; dotsPrint (n-1) end in
  let queenPrint () =
    let rec aux row = begin
      if eq_int row Array.length then () else
      let n = queenArray..(row) in
        dotsPrint(n-1); print_string "Q"; dotsPrint(size - n); print_string "\n"; aux (row + 1)
      end
    withtype {i:nat | i <= Array.length } int(i) -> unit in
    aux(0); print_string "\n" in
  let test j =
    let qj = queenArray..(j) in
    let rec aux i =
      if lt_int i j then
        let qi = queenArray..(i) in
        if qi = qj then false else if abs(qj - qi) = j - i then false else aux (i+1)
      else true
    withtype {i:nat | i < size} int(i) -> bool in aux 0
  withtype {j:nat | j < size} int(j) -> bool in
  let rec loop row =
    let next = queenArray..(row) + 1 in
      if next > Array.length then
        begin assign row 0; if eq_int row 0 then () else loop (row-1) end
      else
      begin assign row next;
        if test row then
          if eq_int (row+1) Array.length then begin queenPrint(); loop(row) end else loop(row+1)
          else loop row
      end
  withtype {row:nat | row < size} int(row) -> unit in loop(0)
withtype int(size) -> unit
;;*)
