qualifier LTs(x) = x < size ar;;
qualifier LEs(x) = x <= size ar;;
qualifier NEs(x) = not(x = size ar);;
qualifier GT0(x) = 0 < x;;
qualifier GE0(x) = 0 <= x;;
qualifier NE0(x) = not(x = 0);;
qualifier EQ0(x) = x = 0;;
qualifier NEG(x) = x < 0;;
qualifier EQm(x) = x = 0-1;;


let f sz = 
	let ar = make sz 0 in
	let sz_minus = sz + 1 in
	let index = sz - sz_minus in
	get ar index
in
let y = get_pos_int () in
	 f y ;;

