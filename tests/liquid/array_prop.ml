qualifier SGT0(x) = 0 < size x;;
qualifier SEQ(x) = size x = size arr;;
qualifier LTs(x) = x < size yarr;;
qualifier LEs(x) = x <= size yarr;;
qualifier NEs(x) = not(x = size yarr);;

qualifier GT0(x) = 0 < x;;
qualifier GE0(x) = 0 <= x;;
qualifier NE0(x) = not(x = 0);;

let outer yarr z = 
	let x i = get yarr i
	in x z
in
let sz = get_pos_int () in
let sz_minus = sz - 1 in
let arr = make sz 0 in
outer arr sz_minus;;
